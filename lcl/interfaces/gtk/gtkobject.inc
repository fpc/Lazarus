{%MainUnit gtkint.pp}
{******************************************************************************
                                   TGtkWidgetSet
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


{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.Create
  Params:  None
  Returns: Nothing

  Contructor for the class.
 ------------------------------------------------------------------------------}
constructor TGtkWidgetSet.Create;
begin
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

  FTimerData  := TList.Create;
  {$IFDEF Use_KeyStateList}
  FKeyStateList_ := TList.Create;
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

{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TGtkWidgetSet.Destroy;
const
  ProcName = '[TGtkWidgetSet.Destroy]';
  GDITYPENAME: array[TGDIType] of String = (
     'gdiBitmap', 'gdiBrush', 'gdiFont', 'gdiPen', 'gdiRegion', 'gdiPalette');
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
  FreeGDKCursors;
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

  if fMessageQueue.HasPaintMessages then begin
    DebugLn(ProcName, Format(rsWarningUnremovedPaintMessages,
      [IntToStr(fMessageQueue.NumberOfPaintMessages)]));
  end;

  if (FDeviceContexts.Count > 0)
  then begin
    DebugLn(ProcName, Format(rsWarningUnreleasedDCsDump,
      [FDeviceContexts.Count]));

    n:=0;
    write(ProcName,'  DCs:  ');
    HashItem:=FDeviceContexts.FirstHashItem;
    while (n<7) and (HashItem<>nil) do
    begin
      DbgOut(' ',HexStr(Cardinal(HashItem^.Item),8));
      HashItem:=HashItem^.Next;
      inc(n);
    end;
    DebugLn();
  end;

  if (FGDIObjects.Count > 0)
  then begin
    DebugLn(ProcName,Format(rsWarningUnreleasedGDIObjectsDump,
       [FGDIObjects.Count]));
    for GDIType := Low(GDIType) to High(GDIType) do
      GDITypeCount[GDIType] := 0;

    n:=0;
    write(ProcName,'   GDIOs:');
    HashItem := FGDIObjects.FirstHashItem;
    while (HashItem <> nil) do
    begin
      if n < 7
      then DbgOut(' ',HexStr(Cardinal(HashItem^.Item),8));

      Inc(GDITypeCount[PGdiObject(HashItem^.Item)^.GDIType]);
      HashItem := HashItem^.Next;
      Inc(n);
    end;
    DebugLn();

    for GDIType := Low(GDIType) to High(GDIType) do
      if GDITypeCount[GDIType] > 0 then
        DebugLn(ProcName,Format('   %s: %d', [GDITYPENAME[GDIType], GDITypeCount[GDIType]]));
  end;


  // tidy up messages
  if FMessageQueue.Count > 0   then begin
    DebugLn(ProcName, Format(rsWarningUnreleasedMessagesInQueue,[FMessageQueue.Count]));
    while FMessageQueue.First<>nil do
      fMessageQueue.RemoveMessage(fMessageQueue.FirstMessageItem,FPMF_All,true);
  end;

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

  // finally remove our loghandler
  g_log_remove_handler(nil, FLogHandlerID);

  GTKWidgetSet := nil;
  
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.SetWindowSizeAndPosition
  Params:  Widget: PGtkWidget; AWinControl: TWinControl
  Returns: Nothing

  Set the size and position of a top level window.
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.SetWindowSizeAndPosition(Window: PGtkWindow;
  AWinControl: TWinControl);
var
  Width, Height: integer;
  //Info: PGtkWindowGeometryInfo;
begin
  Width:=AWinControl.Width;
  // 0 and negative values have a special meaning, so don't use them
  if Width<=0 then Width:=1;
  Height:=AWinControl.Height;
  if Height<=0 then Height:=1;

  //DebugLn('TGtkWidgetSet.SetWindowSizeAndPosition ',AWinControl.Name,':',AWinControl.ClassName,' ',AWinControl.Visible,' Old=',PGtkWidget(Window)^.allocation.Width,',',PGtkWidget(Window)^.allocation.Width,' New=',Width,',',Height);
  // set geometry default size
  //Info:=gtk_window_get_geometry_info(Window, TRUE);
  //if (Info^.default_width<>Width) or (Info^.default_height<>Height) then
    gtk_window_set_default_size(Window,Width,Height);

  // resize
  if assigned(PGtkWidget(Window)^.Window) then
    // widget is realized, resize gdkwindow directly
    gdk_window_move_resize(PGtkWidget(Window)^.Window,AWinControl.Left,
      AWinControl.Top,Width,Height)
  else
    // widget is not yet realized, force resize needed for shrinking under gtk1)
    gtk_widget_set_usize(PGtkWidget(Window), -1,-1);
  //if (PGtkWidget(Window)^.allocation.Width<>Width)
  //and (PGtkWidget(Window)^.allocation.Height<>Height) then begin
    //gtk_widget_set_usize(PGtkWidget(Window), -1,-1);
    gtk_widget_set_usize(PGtkWidget(Window),Width,Height);
  //end;

  // reposition
  {$IFDEF VerboseSizeMsg}
  DebugLn('TGtkWidgetSet.SetWindowSizeAndPosition B ',AWinControl.Name,':',AWinControl.ClassName,
    ' Visible=',dbgs(AWinControl.Visible),
    ' Old=',dbgs(PGtkWidget(Window)^.allocation.X)+','+dbgs(PGtkWidget(Window)^.allocation.Y),
    ' New=',dbgs(AWinControl.Left)+','+dbgs(AWinControl.Top)+','+dbgs(Width)+'x'+dbgs(Height));
  {$ENDIF}
  gtk_widget_set_uposition(PGtkWidget(Window),AWinControl.Left,AWinControl.Top);
end;

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
  AllWindows: TList;
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
        gdk_window_get_user_data(PGDKWindow(List^.Data), @Window);
        if GtkWidgetIsA(PGtkWidget(Window), GTK_TYPE_WINDOW)
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
                Screen.CustomFormIndex(TCustomForm(LCLObject));
          end;
          if AllWindows=nil then AllWindows:=TList.Create;
          AllWindows.Add(ATransientWindow);
        end;
      end;
      list := g_list_next(list);
    end;

    if AllWindows=nil then exit;

    // sort
    // move all modal windows at the end of the window list
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
              ' Wnd=',HexStr(Cardinal(ParentTransientWindow^.GtkWindow),8),
            ' Child=',ATransientWindow^.Component.Name,':',
                               ATransientWindow^.Component.ClassName,
              ' Index=',ATransientWindow^.SortIndex,
              ' Wnd=',HexStr(Cardinal(ATransientWindow^.GtkWindow),8),
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
          ' OldTransientParent=',HexStr(Cardinal(OldTransientParent),8),
          ' Child=',ATransientWindow^.Component.Name,':',
                             ATransientWindow^.Component.ClassName,
            ' Index=',ATransientWindow^.SortIndex,
            ' Wnd=',HexStr(Cardinal(ATransientWindow^.GtkWindow),8),
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
          ' Wnd=',HexStr(Cardinal(ATransientWindow^.GtkWindow),8),
          ' Parent=',HexStr(Cardinal(ATransientWindow^.TransientParent),8),
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
  DbgOut('TGtkWidgetSet.UntransientWindow ',HexStr(Cardinal(GtkWindow),8));
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
    Widget, ParentFixed, ParentWidget: PGtkWidget;
    LCLControl: TControl;
    IsTopLevelWidget: boolean;
    TopologicalList: TList; // list of PGtkWidget;
    i, LCLWidth, LCLHeight: integer;
    WinWidgetInfo: PWinWidgetInfo;

    procedure WriteBigWarning;
    begin
      DebugLn('WARNING: resizing BIG ',
        ' Control=',LCLControl.Name,':',LCLControl.ClassName,
        ' NewSize=',dbgs(LCLWidth),',',dbgs(LCLHeight));
    end;

    procedure RaiseWidgetWithoutControl;
    begin
      RaiseException('ERROR: TGtkWidgetSet.SendCachedLCLMessages Widget '
        +HexStr(Cardinal(Widget),8)+' without LCL control');
    end;

    procedure WriteWarningParentWidgetNotFound;
    begin
      DebugLn('WARNING: TGtkWidgetSet.SendCachedLCLMessages - '
              ,'Parent''s Fixed Widget not found');
      DebugLn('  Control=',LCLControl.Name,':',LCLControl.ClassName,
        ' Parent=',LCLControl.Parent.Name,':',LCLControl.Parent.ClassName,
        ' ParentWidget=',HexStr(Cardinal(ParentWidget),8),
        '');
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
      LCLControl:=TControl(GetLCLObject(Widget));
      if (LCLControl=nil) or (not (LCLControl is TControl)) then begin
        RaiseWidgetWithoutControl;
      end;
      {$IFDEF VerboseSizeMsg}
      if AnsiCompareText(LCLControl.ClassName,'TScrollBar')=0 then
      DebugLn('SendCachedLCLMessages ',LCLControl.Name,':',LCLControl.ClassName,
        ' ',dbgs(LCLControl.Left)+','+dbgs(LCLControl.Top)+','+dbgs(LCLControl.Width)+'x'+dbgs(LCLControl.Height));
      {$ENDIF}

      IsTopLevelWidget:= (LCLControl is TCustomForm)
                         and (LCLControl.Parent = nil);

      if not IsTopLevelWidget then begin
        // resize widget
        LCLWidth:=LCLControl.Width;
        if LCLWidth<=0 then
          LCLWidth:=1;
        LCLHeight:=LCLControl.Height;
        if LCLHeight<=0 then
          LCLHeight:=1;
        if (LCLWidth>10000) or (LCLHeight>10000) then begin
          WriteBigWarning;
        end;
        RealizeWidgetSize(Widget,LCLWidth, LCLHeight);

        // move widget on the fixed widget of parent control
        if (LCLControl.Parent<>nil) and (LCLControl.Parent.HandleAllocated) then
        begin
          ParentWidget:=pgtkWidget(LCLControl.Parent.Handle);
          ParentFixed := GetFixedWidget(ParentWidget);
          if GtkWidgetIsA(ParentFixed,GTK_FIXED_GET_TYPE)
          or GtkWidgetIsA(ParentFixed,GTK_LAYOUT_GET_TYPE) then begin
            FixedMoveControl(ParentFixed, Widget,
                             LCLControl.Left,LCLControl.Top);
          end else begin
            WinWidgetInfo:=GetWidgetInfo(Widget,false);
            if (WinWidgetInfo=nil)
            or (not (wwiNotOnParentsClientArea in WinWidgetInfo^.Flags)) then
              WriteWarningParentWidgetNotFound;
          end;
        end;
      end
      else begin
        // resize form
        {$IFDEF VerboseFormPositioning}
        DebugLn('VFP SendCachedLCLMessages1 ',GetControlWindow(Widget)<>nil);
        if (LCLControl is TCustomForm) then
          DebugLn('VFP SendCachedLCLMessages2 ',LCLControl.ClassName,' ',
            LCLControl.Left,',',LCLControl.Top,',',LCLControl.Width,',',LCLControl.Height);
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

  procedure SendSizeNotificationToLCL(MainWidget: PGtkWidget);
  var
    LCLControl: TWinControl;
    LCLLeft, LCLTop, LCLWidth, LCLHeight: integer;
    GtkLeft, GtkTop, GtkWidth, GtkHeight: integer;
    TopLeftChanged, WidthHeightChanged, IsTopLevelWidget: boolean;
    MessageDelivered: boolean;
    PosMsg : TLMWindowPosChanged;
    SizeMsg: TLMSize;
    MoveMsg: TLMMove;

    procedure UpdateLCLRect;
    begin
      LCLLeft:=LCLControl.Left;
      LCLTop:=LCLControl.Top;
      LCLWidth:=LCLControl.Width;
      LCLHeight:=LCLControl.Height;

      TopLeftChanged:=(LCLLeft<>GtkLeft) or (LCLTop<>GtkTop);
      WidthHeightChanged:=(LCLWidth<>GtkWidth) or (LCLHeight<>GtkHeight);
    end;

  begin
    if not GTK_WIDGET_REALIZED(MainWidget) then begin
      {$IFDEF VerboseSizeMsg}
      DebugLn('SendSizeNotificationToLCL ',LCLControl.Name,':',LCLControl.ClassName,' MainWidget=',WidgetFlagsToString(MainWidget),' Ignored, because not realized ');
      {$ENDIF}
      exit;
    end;
  
    LCLControl:=TWinControl(GetLCLObject(MainWidget));
    if LCLControl=nil then exit;
    {$IFDEF VerboseSizeMsg}
    DebugLn('JJJ1 SendSizeNotificationToLCL ',LCLControl.Name,':',LCLControl.ClassName,' MainWidget=',WidgetFlagsToString(MainWidget));
    {$ENDIF}

    GtkLeft:=MainWidget^.Allocation.X;
    GtkTop:=MainWidget^.Allocation.Y;

    {$Ifdef GTK2}
    if GTK_WIDGET_NO_WINDOW(MainWidget) and GTK_WIDGET_NO_WINDOW(MainWidget^.Parent)
  //   and (not GtkWidgetIsA(MainWidget,GTKAPIWidget_GetType))
    then begin
      Dec(GtkLeft, MainWidget^.parent^.Allocation.X);
      Dec(GtkTop, MainWidget^.parent^.Allocation.Y);
    end;
    {$EndIf}
    GtkWidth:=MainWidget^.Allocation.Width;
    if GtkWidth<0 then GtkWidth:=0;
    GtkHeight:=MainWidget^.Allocation.Height;
    if GtkHeight<0 then GtkHeight:=0;

    IsTopLevelWidget:=(LCLControl is TCustomForm) and (LCLControl.Parent=nil);
    if IsTopLevelWidget then begin
      if not GTK_WIDGET_VISIBLE(MainWidget) then begin
        // size/move messages of invisible windows are not reliable
        // -> ignore
        exit;
      end;
      if GetControlWindow(MainWidget)<>nil then begin
        gdk_window_get_root_origin(GetControlWindow(MainWidget), @GtkLeft, @GtkTop);
      end else begin
        GtkLeft:=LCLControl.Left;
        GtkTop:=LCLControl.Top;
      end;
      {$IFDEF VerboseFormPositioning}
      DebugLn('VFP SendSizeNotificationToLCL ',LCLControl.ClassName,' ',
        GtkLeft,',',GtkTop,',',GtkWidth,',',GtkHeight);
      {$ENDIF}
    end;

    UpdateLCLRect;

    {$IFDEF VerboseSizeMsg}
    DebugLn('JJJ2 ',LCLControl.Name,
      ' GTK=',dbgs(GtkLeft)+','+dbgs(GtkTop)+','+dbgs(GtkWidth)+'x'+dbgs(GtkHeight),
      ' LCL=',dbgs(LCLLeft)+','+dbgs(LCLTop)+','+dbgs(LCLWidth)+'x'+dbgs(LCLHeight)
      );
    {$ENDIF}
    // first send a LM_WINDOWPOSCHANGED message
    if TopLeftChanged or WidthHeightChanged then begin
      PosMsg.Msg := LM_WINDOWPOSCHANGED; //LM_SIZEALLOCATE;
      PosMsg.Result := 0;
      New(PosMsg.WindowPos);
      try
        with PosMsg.WindowPos^ do begin
          hWndInsertAfter := 0;
          x := GtkLeft;
          y := GtkTop;
          cx := GtkWidth;
          cy := GtkHeight;
          flags := 0;
        end;
        MessageDelivered := DeliverMessage(LCLControl, PosMsg) = 0;
      finally
        Dispose(PosMsg.WindowPos);
      end;
      if (not MessageDelivered) then exit;
      if FWidgetsWithResizeRequest.Contains(MainWidget) then exit;
      UpdateLCLRect;
    end;

    // then send a LM_SIZE message
    if WidthHeightChanged then begin
      {$IFDEF VerboseSizeMsg}
      DebugLn('JJJ3 Send LM_SIZE To LCL ',LCLControl.Name,':',LCLControl.ClassName);
      {$ENDIF}
      with SizeMsg do
      begin
        Result := 0;
        Msg := LM_SIZE;
        SizeType := Size_SourceIsInterface;
        Width := SmallInt(GtkWidth);
        Height := SmallInt(GtkHeight);
      end;
      MessageDelivered := (DeliverMessage(LCLControl, SizeMsg) = 0);
      if not MessageDelivered then exit;
      if FWidgetsWithResizeRequest.Contains(MainWidget) then exit;
      UpdateLCLRect;
    end;

    // then send a LM_MOVE message
    if TopLeftChanged then begin
      {$IFDEF VerboseSizeMsg}
      DebugLn('JJJ4 Send LM_MOVE To LCL ',LCLControl.Name,':',LCLControl.ClassName);
      {$ENDIF}
      with MoveMsg do
      begin
        Result := 0;
        Msg := LM_MOVE;
        MoveType := Move_SourceIsInterface;
        XPos := SmallInt(GtkLeft);
        YPos := SmallInt(GtkTop);
      end;
      MessageDelivered := (DeliverMessage(LCLControl,  MoveMsg) = 0);
      if not MessageDelivered then exit;
    end;
  end;

  procedure SendCachedGtkResizeNotifications;
  { This proc sends all cached size messages from the gtk to lcl but in an
    optimized order.
    When sending the LCL a size/move/windowposchanged messages the LCL will
    automatically realign all child controls. This realigning is based on the
    clientrect.
    Therefore, before a size message is sent to the lcl, all clientrect must be
    updated.
    If a size message results in resizing a widget that was also resized, then
    the message for the dependent widget is not sent to the lcl, because the lcl
    resize was after the gtk resize.
  }
  var
    FixWidget, MainWidget: PGtkWidget;
    LCLControl: TWinControl;
    List: TList;
    i: integer;

    procedure RaiseInvalidLCLControl;
    begin
      RaiseException('SendCachedGtkResizeNotifications'
        +' FixWidget='+HexStr(Cardinal(FixWidget),8)
        +' MainWidget='+HexStr(Cardinal(MainWidget),8)
        +' LCLControl='+HexStr(Cardinal(LCLControl),8)
        );
    end;

  begin
    if (FFixWidgetsResized.Count=0) and (FWidgetsResized.Count=0) then exit;

    List:=TList.Create;

    { if any fixed widget was resized then a client area of a LCL control was
      resized
      -> invalidate client rectangles
    }
    {$IFDEF VerboseSizeMsg}
    DebugLn('HHH1 SendCachedGtkClientResizeNotifications Invalidating ClientRects ... '
    ,' FixSizeMsgCount=',dbgs(FFixWidgetsResized.Count));
    {$ENDIF}
    FFixWidgetsResized.AssignTo(List);
    for i:=0 to List.Count-1 do begin
      FixWidget:=List[i];
      MainWidget:=GetMainWidget(FixWidget);
      LCLControl:=TWinControl(GetLCLObject(MainWidget));
      if (LCLControl=nil) or (not (LCLControl is TWinControl)) then
        RaiseInvalidLCLControl;
      LCLControl.InvalidateClientRectCache(false);
    end;

    { if any main widget (= not fixed widget) was resized
      then a LCL control was resized
      -> send WMSize, WMMove, and WMWindowPosChanged messages
    }
    {$IFDEF VerboseSizeMsg}
    DebugLn('HHH2 SendCachedGtkClientResizeNotifications SizeMsgCount=',dbgs(FWidgetsResized.Count));
    {$ENDIF}
    repeat
      MainWidget:=FWidgetsResized.First;
      if MainWidget<>nil then begin
        FWidgetsResized.Remove(MainWidget);
        if not FWidgetsWithResizeRequest.Contains(MainWidget) then begin
          SendSizeNotificationToLCL(MainWidget);
          FixWidget:=GetFixedWidget(MainWidget);
        end;
      end else break;
    until Application.Terminated;

    { if any client area was resized, which MainWidget Size was already in sync
      with the LCL, no message was sent. So, tell each changed client area to
      check its size.
    }
    {$IFDEF VerboseSizeMsg}
    DebugLn('HHH3 SendCachedGtkClientResizeNotifications Updating ClientRects ...');
    {$ENDIF}
    repeat
      FixWidget:=FFixWidgetsResized.First;
      if FixWidget<>nil then begin
        FFixWidgetsResized.Remove(FixWidget);
        MainWidget:=GetMainWidget(FixWidget);
        LCLControl:=TWinControl(GetLCLObject(MainWidget));
        LCLControl.DoAdjustClientRectChange;
      end else begin
        break;
      end;
    until Application.Terminated;

    List.Free;
    {$IFDEF VerboseSizeMsg}
    DebugLn('HHH4 SendCachedGtkClientResizeNotifications  completed.');
    {$ENDIF}
  end;

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
procedure TGtkWidgetSet.SetLabelCaption(const ALabel: PGtkLabel; const ACaption: String; const AComponent: TComponent; const ASignalWidget: PGTKWidget; const ASignal: PChar); 
var
  Caption, Pattern: String;
  AccelKey: Char;
begin
  Caption := ACaption;
  LabelFromAmpersands(Caption, Pattern, AccelKey);

  gtk_label_set_text(ALabel, PChar(Caption));
  
  {$ifdef gtk1}
  gtk_label_set_pattern(ALabel, PChar(Pattern));
  {$endif gtk1}
  
  if AComponent = nil then Exit;
  if ASignalWidget = nil then Exit;
  if ASignal = '' then Exit;
  
  // update the Accelerator
  if AccelKey = #0 
  then Accelerate(AComponent, ASignalWidget, GDK_VOIDSYMBOL, 0, ASignal)
  else Accelerate(AComponent, ASignalWidget, Ord(AccelKey), 0, ASignal);
end;


{------------------------------------------------------------------------------
  procedure TGtkWidgetSet.RealizeWidgetSize(Widget: PGtkWidget; NewWidth,
    NewHeight: integer);
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.RealizeWidgetSize(Widget: PGtkWidget; NewWidth,
  NewHeight: integer);
var
  Requisition: TGtkRequisition;
  {$IFDEF VerboseSizeMsg}
  LCLObject: TObject;
  {$ENDIF}
  FixedWidget: PGtkWidget;
begin
  if NewWidth<=0 then NewWidth:=1;
  if NewHeight<=0 then NewHeight:=1;

  {$IFDEF VerboseSizeMsg}
  LCLObject:=GetNearestLCLObject(Widget);
  DbgOut('TGtkWidgetSet.RealizeWidgetSize Widget='+HexStr(Cardinal(Widget),8)+WidgetFlagsToString(Widget)+
    ' New='+dbgs(NewWidth)+','+dbgs(NewHeight));
  if (LCLObject<>nil) and (LCLObject is TControl) then begin
    with TControl(LCLObject) do
      DebugLn(' LCL=',Name,':',ClassName,' ',dbgs(Left),',',dbgs(Top),',',dbgs(Width),',',dbgs(Height));
  end else begin
    DebugLn(' LCL=',HexStr(Cardinal(LCLObject),8));
  end;
  {$ENDIF}

  if GtkWidgetIsA(Widget,GTK_TYPE_SCROLLBAR) then
  begin
    // the width of a scrollbar is fixed and depends only on the theme
    gtk_widget_size_request(widget, @Requisition);
    if GtkWidgetIsA(Widget, GTK_TYPE_HSCROLLBAR) then
    begin
      NewHeight:=Requisition.height;
    end else begin
      NewWidth:=Requisition.width;
    end;
    //DebugLn('TGtkWidgetSet.RealizeWidgetSize A ',Newwidth,',',Newheight);
  end;

  gtk_widget_set_usize(Widget, NewWidth, NewHeight);

  if GtkWidgetIsA(Widget, GTK_TYPE_COMBO) then
  begin
    // the combobox has an entry, which height is not resized
    // automatically. Do it manually.
    gtk_widget_set_usize(PGtkCombo(Widget)^.entry,
      PGtkCombo(Widget)^.entry^.allocation.width, NewHeight);
  end;

  if GtkWidgetIsA(Widget,gtk_toolbar_get_type) then begin
    FixedWidget:=GetFixedWidget(Widget);
    if (FixedWidget<>nil) and (FixedWidget<>Widget) then begin
      //DebugLn('WARNING: ToDo TGtkWidgetSet.RealizeWidgetSize for TToolBar ',NewWidth,',',NewHeight);
      gtk_widget_set_usize(FixedWidget,NewWidth,NewHeight);
    end;
  end;
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
    WindowList: TList;
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
      Context.WindowList:=TList.Create;
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
      ' InternalWindow=',HexStr(Cardinal(PaintWindow),8),
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
    ' ',HexStr(Cardinal(Context.MainWidget),8),
    ' ',HexStr(Cardinal(Context.MainWindow),8),
    ' ',HexStr(Cardinal(Context.ClientWidget),8),
    ' ',HexStr(Cardinal(Context.ClientWindow),8),
    '');}

  ForAllChilds(Context.MainWidget);

  Context.WindowList.Free;
end;

{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.HandleEvents
  Params:  None
  Returns: Nothing

  Handle all pending messages of the GTK engine and of this interface
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.HandleEvents;

  function PendingGtkMessagesExists: boolean;
  begin
    Result:=(gtk_events_pending<>0) or LCLtoGtkMessagePending;
  end;

var

  vlItem : TGtkMessageQueueItem;
  vlMsg  : PMSg;

begin
  repeat
    // send cached LCL messages to the gtk
    SendCachedLCLMessages;

    // let gtk handle all its messages and call our callbacks
    while gtk_events_pending<>0 do
      gtk_main_iteration_do(False);

    // send cached gtk messages to the lcl
    SendCachedGtkMessages;

    // then handle our own messages
    while not Application.Terminated do begin
      // fetch first message
      vlItem := fMessageQueue.FirstMessageItem;
      if vlItem = nil then break;

      // remove message from queue
      if vlItem.IsPaintMessage then begin
        // paint messages are the most expensive messages in the LCL,
        // therefore they are sent always after all other
        if fMessageQueue.HasNonPaintMessages then begin
          // there are non paint messages -> keep paint message back
          fMessageQueue.MoveToLast(FMessageQueue.First);
          continue;
        end else begin
          // there are only paint messages left in the queue
          // -> check other queues
          if PendingGtkMessagesExists then break;
        end;
      end;

      vlMsg:=fMessageQueue.PopFirstMessage;

      // Send message
      with vlMsg^ do SendMessage(hWND, Message, WParam, LParam);
      Dispose(vlMsg);
    end;

    // proceed until all messages are handled
  until (not PendingGtkMessagesExists) or Application.Terminated;
end;

{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.WaitMessage
  Params:  None
  Returns: Nothing

  Passes execution control to the GTK engine till something happens
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.WaitMessage;
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
  // MG: TODO: call FillScreenFonts on demand, not for every application
  //FillScreenFonts(Screen.Fonts);
  InitKeyboardTables;
  { Compute pixels per inch variable }
  ScreenInfo.PixelsPerInchX :=
    RoundToInt(gdk_screen_width / (gdk_screen_width_mm / 25.4));
  ScreenInfo.PixelsPerInchY :=
    RoundToInt(gdk_screen_height / (gdk_screen_height_mm / 25.4));
  ScreenInfo.ColorDepth := gdk_visual_get_system^.depth;
end;

{------------------------------------------------------------------------------
  Method: TGtkWidgetSet.AppMinimize
  Params:  None
  Returns: Nothing

  Minimizes the application
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.AppMinimize;
begin
  // TODO: Implement me!
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
function TGtkWidgetSet.CreateTimer(Interval: integer; TimerFunc: TFNTimerProc) : integer;
var
  TimerInfo: PGtkITimerinfo;
begin
  if ((Interval < 1) or (TimerFunc = nil))
  then
    Result := 0
  else begin
    New(TimerInfo);
    TimerInfo^.TimerFunc := TimerFunc;
    {$IFDEF VerboseTimer}
    DebugLn('TGtkWidgetSet.SetTimer ',HexStr(Cardinal(TimerInfo),8),' CurTimerCount=',FTimerData.Count,' OldTimerCount=',FOldTimerData.Count);
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
function TGtkWidgetSet.DestroyTimer(TimerHandle: integer) : boolean;
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
      DebugLn('TGtkWidgetSet.KillTimer TimerInfo=',HexStr(Cardinal(TimerInfo),8),' TimerHandle=',TimerHandle,' CurTimerCount=',FTimerData.Count,' OldTimerCount=',FOldTimerData.Count);
      {$ENDIF}
      gtk_timeout_remove(TimerInfo^.TimerHandle);
      FTimerData.Delete(n);
      Dispose(TimerInfo);
    end;
  end;
  Result:=true;
end;

procedure TGtkWidgetSet.LoadFromXPMFile(Bitmap: TObject; Filename: PChar);
var
  GdiObject: PGdiObject;
  GDKColor: TGDKColor;
  Window: PGdkWindow;
  ColorMap: PGdkColormap;
  P: Pointer;
  TheBitmap: TBitmap;
  Width, Height, Depth : Longint;
begin
  if not (Bitmap is TBitmap) then
    RaiseException('TGtkWidgetSet.LoadFromXPMFile Bitmap is not TBitmap: '
                   +Bitmap.ClassName);
  TheBitmap:=TBitmap(Bitmap);
  GdiObject := NewGDIObject(gdiBitmap);
  if TheBitmap.TransparentColor<>clNone then begin
    GDKColor := AllocGDKColor(ColorToRGB(TheBitmap.TransparentColor));
    p := @GDKColor;
  end else
    p:=nil; // automatically create transparency mask
  Window:=nil; // use the X root window for colormap

  {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}

  if Window<>nil then
    ColorMap:=gdk_window_get_colormap(Window)
  else
    ColorMap:=gdk_colormap_get_system;
  GdiObject^.GDIPixmapObject :=
    gdk_pixmap_colormap_create_from_xpm(Window,Colormap,
           GdiObject^.GDIBitmapMaskObject, p, Filename);
  GdiObject^.GDIBitmapType:=gbPixmap;
  gdk_drawable_get_size(GdiObject^.GDIPixmapObject,@Width, @Height);
  Depth := gdk_drawable_get_depth(GdiObject^.GDIPixmapObject);

 If GdiObject^.Visual <> nil then
   GDK_Visual_UnRef(GdiObject^.Visual);

 If GdiObject^.Colormap <> nil then
   GDK_Colormap_UnRef(GdiObject^.Colormap);

  GdiObject^.Visual := gdk_window_get_visual(GdiObject^.GDIpixmapObject);
  If GdiObject^.Visual = nil then
    GdiObject^.Visual := gdk_visual_get_best_with_depth(Depth)
  else
    gdk_visual_ref(GdiObject^.Visual);

  GdiObject^.Colormap := gdk_colormap_new(GdiObject^.Visual, GdkTrue);
  {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}

  TheBitmap.Handle := HBITMAP(GdiObject);
  If GdiObject^.GDIBitmapMaskObject <> nil then
    TheBitmap.Transparent := True
  else
    TheBitmap.Transparent := False;
end;

procedure TGtkWidgetSet.LoadFromPixbufFile(Bitmap: TObject; Filename: PChar);
{$Ifndef NoGdkPixbufLib}
var
  TheBitmap: TBitmap;
{$ENDIF}

  function LoadFile: Boolean;
  {$Ifndef NoGdkPixbufLib}
  var
    Src : PGDKPixbuf;
    Tmp : hBitmap;
    Width, Height,
    Depth : Longint;
  begin
    Result := False;
    SRC := nil;
    SRC := gdk_pixbuf_new_from_file(FileName{$IFDEF Gtk2},nil{$ENDIF});
    If SRC = nil then
      exit;
    Width := gdk_pixbuf_get_width(Src);
    Height := gdk_pixbuf_get_height(Src);

    TMP := CreateCompatibleBitmap(0, Width, Height);
    gdk_pixbuf_render_pixmap_and_mask(Src,
      PGDIObject(TMP)^.GDIPixmapObject,
      PGDIObject(TMP)^.GDIBitmapMaskObject,
      0);

    {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
    Depth := gdk_drawable_get_depth(PGDIObject(TMP)^.GDIPixmapObject);

    If PGDIObject(TMP)^.Visual <> nil then
      GDK_Visual_UnRef(PGDIObject(TMP)^.Visual);

    PGDIObject(TMP)^.Visual := gdk_window_get_visual(PGDIObject(TMP)^.GDIPixmapObject);
    If PGDIObject(TMP)^.Visual = nil then
      PGDIObject(TMP)^.Visual := gdk_visual_get_best_with_depth(Depth)
    else
      GDK_Visual_Ref(PGDIObject(TMP)^.Visual);

    If PGDIObject(TMP)^.Colormap <> nil then
      GDK_Colormap_UnRef(PGDIObject(TMP)^.Colormap);

    PGDIObject(TMP)^.Colormap :=
                             gdk_colormap_new(PGDIObject(TMP)^.Visual, GdkTrue);

    {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
    TheBitmap.Handle := TMP;
    GDK_Pixbuf_Unref(Src);
    Result := True;
  {$Else not NoGdkPixbufLib}
  begin
    DebugLn('WARNING: [TGtkWidgetSet.LoadFromPixbufFile] GDKPixbuf support has been disabled, unable to load files!');
    Result := True;
  {$EndIf}
  end;

begin
  if not (Bitmap is TBitmap) then
    RaiseException('TGtkWidgetSet.LoadFromPixbufFile Bitmap is not TBitmap: '
                   +Bitmap.ClassName);
  TheBitmap:=TBitmap(Bitmap);
  if not LoadFile then
    DebugLn('WARNING: [TGtkWidgetSet.LoadFromPixbufFile] loading file FAILED!');
end;

procedure TGtkWidgetSet.LoadFromPixbufData(Bitmap : hBitmap; Data : PByte);
Type
  TBITMAPHEADER = packed record
    FileHeader : tagBitmapFileHeader;
    InfoHeader : tagBitmapInfoHeader;
  end;

  Procedure FillBitmapInfo(Bitmap : hBitmap; Var Header : TBitmapHeader);
  var
    DIB : TDIBSection;
    BitmapHeader : TagBITMAPINFO;
  begin
    FillChar(DIB, SizeOf(DIB), 0);
    GetObject(Bitmap, SizeOf(DIB), @DIB);
    BitmapHeader.bmiHeader := DIB.dsbmih;
    With Header, Header.FileHeader, Header.InfoHeader do begin
      InfoHeader := BitmapHeader.bmiHeader;
      FillChar(FileHeader, sizeof(FileHeader), 0);
      bfType      := $4D42;
      bfSize      := SizeOf(Header) + biSizeImage;
      bfOffBits   := SizeOf(Header);
    end;
  end;

  function LoadData : Boolean;
  {$Ifndef NoGdkPixbufLib}
  var
    Loader : PGdkPixbufLoader;
    Src : PGDKPixbuf;
    BMPInfo : TBitmapHeader;
  begin
    Result := False;

    FillBitmapInfo(Bitmap, BMPInfo);

    Loader := gdk_pixbuf_loader_new;
    If Loader = nil then
      exit;

    SRC := nil;

    If gdk_pixbuf_loader_write(Loader, TGdkPixBufBuffer(@BMPInfo),
                      SizeOf(BMPInfo) div SizeOf(Char){$IFDEF Gtk2},nil{$ENDIF})
    then begin
      If gdk_pixbuf_loader_write(Loader, TGdkPixBufBuffer(Data),
        BMPInfo.InfoHeader.biSizeImage{$IFDEF Gtk2},nil{$ENDIF}) then
      begin
        SRC := gdk_pixbuf_loader_get_pixbuf(loader);
        if Src=nil then
          DebugLn('WARNING: [TGtkWidgetSet.LoadFromPixbufData] Error occured loading Pixbuf!');
      end
      else
        DebugLn('WARNING: [TGtkWidgetSet.LoadFromPixbufData] Error occured loading Image!');
    end
    else
      DebugLn('WARNING: [TGtkWidgetSet.LoadFromPixbufData] Error occured loading Bitmap Header!');
    gdk_pixbuf_loader_close(Loader{$IFDEF Gtk2},nil{$ENDIF});

    If SRC = nil then
      exit;

    With PGDIObject(Bitmap)^ do begin
      {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
      gdk_pixbuf_render_pixmap_and_mask(Src,
        GDIPixmapObject,
        GDIBitmapMaskObject,
        0);


      Depth := gdk_drawable_get_depth(GDIPixmapObject);

      If Visual <> nil then
        GDK_Visual_UnRef(Visual);

      Visual := gdk_window_get_visual(GDIPixmapObject);
      If Visual = nil then
        Visual := gdk_visual_get_best_with_depth(Depth)
      else
        GDK_Visual_Ref(Visual);

      If Colormap <> nil then
        GDK_Colormap_UnRef(Colormap);

      Colormap := gdk_colormap_new(Visual, GdkTrue);

      {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}

      GDK_Pixbuf_Unref(Src);
    end;

    Result := True;
  {$Else not NoGdkPixbufLib}
  begin
    DebugLn('WARNING: [TGtkWidgetSet.LoadFromPixbufData] GDKPixbuf support has been disabled, unable to load data!');
    Result := True;
  {$EndIf}
  end;

begin
  if not LoadData then
    DebugLn('WARNING: [TGtkWidgetSet.LoadFromPixbufData] loading data FAILED!');
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.InternalGetDIBits(DC: HDC; Bitmap: HBitmap;
    StartScan, NumScans: UINT;
    BitSize : Longint; Bits: Pointer;
    var BitInfo: BitmapInfo; Usage: UINT; DIB : Boolean): Integer;
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.InternalGetDIBits(DC: HDC; Bitmap: HBitmap;
  StartScan, NumScans: UINT;
  BitSize : Longint; Bits: Pointer;
  var BitInfo: BitmapInfo; Usage: UINT; DIB : Boolean): Integer;
const
  PadLine : array[0..12] of Byte = (0,0,0,0,0,0,0,0,0,0,1,0,0);
  TempBuffer : array[0..2] of Byte = (0,0,0);
var
  {$IfNDef NoGDKPixbuflib}
  Source: PGDKPixbuf;
  rowstride, PixelPos: Longint;
  Pixels: PByte;
  {$Else}
  Source: PGDKImage;//The MONDO slow way...
  {$EndIf}
  FDIB: TDIBSection;
  X, Y: Longint;
  PadSize, Pos, BytesPerPixel: Longint;
  Buf16Bit: word;

  Procedure DataSourceInitialize(Bitmap : PGDIObject; Width : Longint);
  begin
    Source := nil;

    case Bitmap^.GDIBitmapType of
      gbBitmap:
        If Bitmap^.GDIBitmapObject <> nil then begin
         {$IfNDef NoGDKPixbuflib}
           Source := gdk_pixbuf_get_from_drawable(nil, Bitmap^.GDIBitmapObject,
             Bitmap^.Colormap,0,StartScan,0,0,Width,StartScan + NumScans);
           rowstride := gdk_pixbuf_get_rowstride(Source);
           Pixels := PByte(gdk_pixbuf_get_pixels(Source));
         {$else}
           {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
           Source := gdk_image_get(Bitmap^.GDIBitmapObject, 0, StartScan, Width,
             StartScan + NumScans);
         {$EndIf}
        end;
      gbPixmap:
        If Bitmap^.GDIPixmapObject <> nil then begin
         {$IfNDef NoGDKPixbuflib}
           Source := gdk_pixbuf_get_from_drawable(nil, Bitmap^.GDIPixmapObject,
             Bitmap^.Colormap,0,StartScan,0,0,Width,StartScan + NumScans);
           rowstride := gdk_pixbuf_get_rowstride(Source);
           Pixels := PByte(gdk_pixbuf_get_pixels(Source));
         {$else}
           {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
           Source := gdk_image_get(Bitmap^.GDIPixmapObject, StartScan, 0, Width,
             StartScan + NumScans);
         {$EndIf}
        end;
      {obsolete: gbImage :
        If Bitmap^.GDI_RGBImageObject <> nil then begin
          DebugLn('WARNING : [TGtkWidgetSet.GetDIBits] support for gdiImage unimplimented!.');
        end;}
    end;
  end;

  Function DataSourceGetGDIRGB(Bitmap : PGDIObject; X, Y : Longint) : TGDIRGB;
  {$IfNDef NoGDKPixbuflib}
  begin
     If Bitmap <> nil then ; //Keep compiler happy..

     PixelPos := rowstride*Y + X*3;

     With Result do begin
       Red := Pixels[PixelPos + 0];
       Green := Pixels[PixelPos + 1];
       Blue := Pixels[PixelPos + 2];
     end;

  {$else}
  var
    Pixel : Longint;
  begin
    Pixel := 0;

    {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}

    Pixel := gdk_image_get_pixel(Source, X, Y);

    Result := GDKPixel2GDIRGB(Pixel, Bitmap^.Visual, Bitmap^.Colormap);
  {$EndIf}
  end;

  Procedure DataSourceFinalize;
  begin
  {$IfNDef NoGDKPixbuflib}
    GDK_Pixbuf_Unref(Source);
  {$else}
    {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
    gdk_image_destroy(Source);
  {$EndIf}
  end;

  Procedure WriteData(Value : PByte; Size : Longint);
  var
    I : Longint;
  begin
    For I := 0 to Size - 1 do
      PByte(Bits)[Pos + I] := Value[I];
    Inc(Pos, Size);
  end;

  Procedure WriteData(Value : Word);
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
  if IsValidGDIObject(Bitmap)
  then begin
    case PGDIObject(Bitmap)^.GDIType of
    gdiBitmap:
      begin
        FillChar(FDIB, SizeOf(FDIB), 0);
        GetObject(Bitmap, SizeOf(FDIB), @FDIB);
        BitInfo.bmiHeader := FDIB.dsBmih;

        With PGDIObject(Bitmap)^, BitInfo.bmiHeader do begin
          If not DIB then begin
            NumScans := biHeight;
            StartScan := 0;
          end;
          BytesPerPixel:=biBitCount div 8;

          {DebugLn('TGtkWidgetSet.InternalGetDIBits A BitSize=',BitSize,
          ' biSizeImage=',biSizeImage,' biHeight=',biHeight,' biWidth=',biWidth,
          ' NumScans=',NumScans,' StartScan=',StartScan,
          ' Bits=',HexStr(Cardinal(Bits),8),' MemSize(Bits)=',MemSize(Bits),
          ' biBitCount=',biBitCount);}
          If BitSize <= 0 then
            BitSize := longint(SizeOf(Byte))
                       *(longint(biSizeImage) div biHeight)
                       *longint(NumScans + StartScan);
          If MemSize(Bits) < BitSize then begin
            DebugLn('WARNING: [TGtkWidgetSet.InternalGetDIBits] not enough memory allocated for Bits!');
            exit;
          end;
          // ToDo: other bitcounts
          if (biBitCount<>24) and (biBitCount<>16) then begin
            DebugLn('WARNING: [TGtkWidgetSet.InternalGetDIBits] unsupported biBitCount=',dbgs(biBitCount));
            exit;
          end;
          Pos := 0;
          PadSize := (Longint(biSizeImage) div biHeight)
                     - biWidth*BytesPerPixel;
          DataSourceInitialize(PGDIObject(Bitmap), biWidth);
          if NumScans - 1<>0 then begin
            If DIB then begin
              Y:=NumScans - 1;
            end else begin
              Y:=0;
            end;
            repeat
              if biBitCount=24 then begin
                for X := 0 to biwidth - 1 do begin
                  With DataSourceGetGDIRGB(PGDIObject(Bitmap), X, Y) do begin
                    TempBuffer[0]  := Blue;
                    TempBuffer[1] := Green;
                    TempBuffer[2]   := Red;
                  end;
                  WriteData(TempBuffer, BytesPerPixel);
                end;
              end else if biBitCount=16 then begin
                for X := 0 to biwidth - 1 do begin
                  With DataSourceGetGDIRGB(PGDIObject(Bitmap), X, Y) do begin
                    Buf16Bit:=(Blue shr 3) shl 11
                             +(Green shr 2) shl 5
                             +(Red shr 3);
                  end;
                  WriteData(Buf16Bit);
                end;
              end;
              WriteData(PadLine, PadSize);
              If DIB then begin
                dec(y);
                if Y<=0 then break;
              end else begin
                inc(y);
                if Y>=longint(NumScans) - 1 then break;
              end;
            until false;
          end
        end;
        DataSourceFinalize;
      end;
    else
      DebugLn('WARNING: [TGtkWidgetSet.InternalGetDIBits] not a Bitmap!');
    end;
  end
  else
    DebugLn('WARNING: [TGtkWidgetSet.InternalGetDIBits] invalid Bitmap!');
  {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
end;

function TGtkWidgetSet.GetWindowRawImageDescription(GDKWindow: PGdkWindow;
  Desc: PRawImageDescription): boolean;
var
  Visual: PGdkVisual;
  Width, Height: integer;
//  WindowType: TGdkWindowType;
  IsGdkBitmap: Boolean;
begin
  Result := false;
  if Desc=nil then begin
    RaiseGDBException('TGtkWidgetSet.GetWindowRawImageDescription');
    exit;
  end;

  Visual:=nil;
  Width:=0;
  Height:=0;
  IsGdkBitmap:=false;

  If GDKWindow <> nil then begin
    Visual:=gdk_window_get_visual(GDKWindow);
    GDK_Window_Get_Size(GDKWindow,@Width,@Height);
//    if Visual=nil then begin
//      WindowType:=gdk_window_get_type(GDKWindow);
//      if WindowType=GDK_WINDOW_PIXMAP then begin
        // a pixmap without visual
        //DebugLn('TGtkWidgetSet.GetWindowRawImageDescription GdkBitmap Type=',WindowType,' ',Width,',',Height,' ',GDK_WINDOW_PIXMAP);
        // ToDo: find a test: gdkpixmap or gdkbitmap
        //if IsBitmap then IsGdkBitmap:=true;
//      end;
//    end;
  end;
  if Visual=nil then begin
    Visual := GDK_Visual_Get_System;
    if Visual=nil then exit;
  end;

  FillChar(Desc^,SizeOf(TRawImageDescription),0);

  // Format
  if IsGdkBitmap then begin
    Desc^.Format:=ricfGray;
  end else begin
    case Visual^.thetype of
    GDK_VISUAL_STATIC_GRAY:  Desc^.Format:=ricfGray;
    GDK_VISUAL_GRAYSCALE:    Desc^.Format:=ricfGray;
    GDK_VISUAL_STATIC_COLOR: Desc^.Format:=ricfGray;
    GDK_VISUAL_PSEUDO_COLOR: Desc^.Format:=ricfGray;
    GDK_VISUAL_TRUE_COLOR:   Desc^.Format:=ricfRGBA;
    GDK_VISUAL_DIRECT_COLOR: Desc^.Format:=ricfRGBA;
    else
      DebugLn('TGtkWidgetSet.GetWindowRawImageDescription unknown Visual type ',
              dbgs(Integer(Visual^.thetype)));
      exit;
    end;
  end;

  // Palette
  Desc^.HasPalette:=(not IsGdkBitmap)
    and (Visual^.thetype in [GDK_VISUAL_GRAYSCALE,
                              GDK_VISUAL_STATIC_COLOR,GDK_VISUAL_PSEUDO_COLOR]);
  // Depth
  if IsGdkBitmap then
    Desc^.Depth:=1
  else
    Desc^.Depth:=Visual^.Depth;
  // Width + Height
  Desc^.Width:=cardinal(Width);
  Desc^.Height:=cardinal(Height);
  // PaletteEntries
  if Desc^.HasPalette then begin
    // ToDo
    Desc^.PaletteColorCount:=0;
  end else
    Desc^.PaletteColorCount:=0;
  // BitOrder
  Desc^.BitOrder:=riboBitsInOrder;
  // ByteOrder
  if Visual^.byte_order=GDK_MSB_FIRST then
    Desc^.ByteOrder:=riboMSBFirst
  else
    Desc^.ByteOrder:=riboLSBFirst;
  // LineOrder
  Desc^.LineOrder:=riloTopToBottom;
  // ColorCount
  Desc^.ColorCount:=0;
  // BitsPerPixel
  case Desc^.Depth of                       //TODO MWE: Isn't this Visual^.bits_per_rgb
  0..8:   Desc^.BitsPerPixel:=Desc^.Depth;
  9..16:  Desc^.BitsPerPixel:=16;
  17..32: Desc^.BitsPerPixel:=32;
  else    Desc^.BitsPerPixel:=64;
  end;
  // LineEnd
  case Desc^.Depth of
  0..8:   Desc^.LineEnd:=rileByteBoundary;
  9..32:  Desc^.LineEnd:=rileDWordBoundary; // On X11 i386 lines are never aligned to words, only to dwords
  else    Desc^.LineEnd:=rileQWordBoundary;
  end;
  // Precisions and Shifts
  if IsGdkBitmap then begin
    Desc^.RedPrec:=1;
    Desc^.RedShift:=0;
  end else begin
    Desc^.RedPrec:=Visual^.red_prec;
    Desc^.RedShift:=Visual^.red_shift;
    Desc^.GreenPrec:=Visual^.green_prec;
    Desc^.GreenShift:=Visual^.green_shift;
    Desc^.BluePrec:=Visual^.blue_prec;
    Desc^.BlueShift:=Visual^.blue_shift;
    Desc^.AlphaSeparate:=true;
    Desc^.AlphaPrec:=1;
    Desc^.AlphaShift:=0;
  end;
  // AlphaBitsPerPixel and AlphaLineEnd
  Desc^.AlphaBitsPerPixel:=Desc^.AlphaPrec;
  Desc^.AlphaLineEnd:=rileByteBoundary;
  Desc^.AlphaBitOrder:=riboBitsInOrder;
  Desc^.AlphaByteOrder:=riboLSBFirst;

  {$IFDEF VerboseRawImage}
  DebugLn('TGtkWidgetSet.GetWindowRawImageDescription A ',RawImageDescriptionAsString(Desc));
  {$ENDIF}

  Result:=true;
end;

function TGtkWidgetSet.GetRawImageFromGdkWindow(GDKWindow: PGdkWindow;
  MaskBitmap: PGdkBitmap; const SrcRect: TRect;
  var NewRawImage: TRawImage): boolean;
var
  ARect: TRect;
  MaxRect: TRect;
  SourceRect: TRect;
  AnImage: PGdkImage;
begin
  Result:=false;
  FillChar(NewRawImage,SizeOf(NewRawImage),0);
  if GdkWindow=nil then
    RaiseGDBException('TGtkWidgetSet.GetRawImageFromGdkWindow');

  // get raw image description
  {$IFDEF VerboseRawImage}
  DebugLn('TGtkWidgetSet.GetRawImageFromGdkWindow Get Desc GdkWindow=',HexStr(Cardinal(GdkWindow),8));
  {$ENDIF}
  if not GetWindowRawImageDescription(GdkWindow,@NewRawImage.Description) then
  begin
    DebugLn('WARNING: TGtkWidgetSet.GetRawImageFromGdkWindow GetWindowRawImageDescription failed');
    exit;
  end;

  // get intersection
  ARect:=SrcRect;
  {$IFDEF VerboseRawImage}
  DebugLn('TGtkWidgetSet.GetRawImageFromGdkWindow Intersect ARect=',
    dbgs(ARect.Left),',',dbgs(ARect.Top),',',dbgs(ARect.Right),',',dbgs(ARect.Bottom),
    ' DevW=',dbgs(NewRawImage.Description.Width),' DevH=',dbgs(NewRawImage.Description.Height));
  {$ENDIF}
  MaxRect:=Rect(0,0,NewRawImage.Description.Width,
                    NewRawImage.Description.Height);
  SourceRect:=ARect;
  IntersectRect(SourceRect,ARect,MaxRect);
  NewRawImage.Description.Width:=SourceRect.Right-SourceRect.Left;
  NewRawImage.Description.Height:=SourceRect.Bottom-SourceRect.Top;
  {$IFDEF VerboseRawImage}
  DebugLn('TGtkWidgetSet.GetRawImageFromGdkWindow get image ',
    dbgs(SourceRect.Left),',',dbgs(SourceRect.Top),',',dbgs(SourceRect.Right),',',dbgs(SourceRect.Bottom),
    ' GDKWindow=',HexStr(Cardinal(GDkWindow),8));
  {$ENDIF}
  if (NewRawImage.Description.Width<=0) or (NewRawImage.Description.Height<=0)
  then begin
    DebugLn('WARNING: TGtkWidgetSet.GetRawImageFromGdkWindow Intersection empty');
    exit;
  end;

  if NewRawImage.Description.Depth=1 then begin
    RaiseException('TGtkWidgetSet.GetRawImageFromGdkWindow Depth=1 invalid');
    exit;
  end;

  // get gdk_image
  AnImage:=gdk_image_get(GDKWindow,SourceRect.Left,SourceRect.Top,
                         NewRawImage.Description.Width,
                         NewRawImage.Description.Height);
  if AnImage=nil then begin
    DebugLn('WARNING: TGtkWidgetSet.GetRawImageFromGdkWindow gdk_image_get failed');
    exit;
  end;
  try
    // consistency checks
    if NewRawImage.Description.Depth<>AnImage^.Depth then
      RaiseGDBException('NewRawImage.Description.Depth<>AnImage^.Depth '+IntToStr(NewRawImage.Description.Depth)+'<>'+IntToStr(AnImage^.Depth));
    if NewRawImage.Description.BitsPerPixel<>AnImage^.bpp then
      RaiseGDBException('NewRawImage.Description.BitsPerPixel<>AnImage^.bpp');

    NewRawImage.DataSize:=AnImage^.bpl * AnImage^.Height;
    {$IFDEF VerboseRawImage}
    DebugLn('TGtkWidgetSet.GetRawImageFromGdkWindow G Width=',dbgs(AnImage^.Width),' Height=',dbgs(AnImage^.Height),
      ' BitsPerPixel=',dbgs(NewRawImage.Description.BitsPerPixel),' bpl=',dbgs(AnImage^.bpl));
    {$ENDIF}
    if NewRawImage.DataSize<>cardinal(AnImage^.bpl) * AnImage^.Height then
      RaiseGDBException('NewRawImage.DataSize<>AnImage^.bpl*AnImage^.Height');

    // copy data
    NewRawImage.Description.Width:=AnImage^.Width;
    NewRawImage.Description.Height:=AnImage^.Height;

    { i:=0;
    for y:=0 to AnImage^.Height-1 do begin
      for x:=0 to AnImage^.Width-1 do begin
        AColor:=gdk_image_get_pixel(AnImage,x,y);
        pGuint(NewRawImage.Data)[i]:=AColor;
        if (y=5) then DbgOut(' ',HexStr(Cardinal(AColor),8),'@',HexStr(Cardinal(@pGuint(NewRawImage.Data)[i]),8));
        inc(i);
      end;
    end;
    DebugLn('');}
    ReAllocMem(NewRawImage.Data,NewRawImage.DataSize);
    if NewRawImage.DataSize>0 then
      System.Move(AnImage^.Mem^,NewRawImage.Data^,NewRawImage.DataSize);

    {$IFDEF VerboseRawImage}
    DebugLn('TGtkWidgetSet.GetRawImageFromGdkWindow H ',
      ' Width=',dbgs(NewRawImage.Description.Width),
      ' Height=',dbgs(NewRawImage.Description.Height),
      ' Depth=',dbgs(NewRawImage.Description.Depth),
      ' DataSize=',dbgs(NewRawImage.DataSize));
    {$ENDIF}
  finally
    gdk_image_destroy(AnImage);
  end;
  
  if MaskBitmap<>nil then begin
    // get mask
    {$IFDEF VerboseRawImage}
    DebugLn('TGtkWidgetSet.GetRawImageFromGdkWindow get mask ',dbgs(SourceRect.Left),',',dbgs(SourceRect.Top),',',dbgs(SourceRect.Right),',',dbgs(SourceRect.Bottom),' MaskBitmap=',HexStr(Cardinal(MaskBitmap),8));
    {$ENDIF}
    if not GetRawImageMaskFromGdkBitmap(MaskBitmap,SourceRect,NewRawImage) then
      exit;
  end;

  Result:=true;
end;

function TGTKWidgetSet.GetRawImageMaskFromGdkBitmap(MaskBitmap: PGdkBitmap;
  const SrcRect: TRect; var RawImage: TRawImage): boolean;
// SrcRect must ly completely in the MaskBitmap
var
  Width, Height: cardinal;
  AnImage: PGdkImage;
begin
  Result:=false;

  Width:=SrcRect.Right-SrcRect.Left;
  Height:=SrcRect.Bottom-SrcRect.Top;

  // check consistency
  if not RawImage.Description.AlphaSeparate then
    RaiseException('TGTKWidgetSet.GetRawImageMaskFromGdkBitmap RawImage.Description.AlphaSeparate=false');
  if (Width<>RawImage.Description.Width) then
    RaiseException('TGTKWidgetSet.GetRawImageMaskFromGdkBitmap Width<>RawImage.Description.Width');
  if (Height<>RawImage.Description.Height) then
    RaiseException('TGTKWidgetSet.GetRawImageMaskFromGdkBitmap Height<>RawImage.Description.Height');
  if RawImage.Mask<>nil then
    RaiseException('TGTKWidgetSet.GetRawImageMaskFromGdkBitmap RawImage.Mask<>nil');

  // get gdk_image from gdkbitmap
  AnImage:=gdk_image_get(MaskBitmap,SrcRect.Left,SrcRect.Top,Width,Height);
  if AnImage=nil then begin
    DebugLn('WARNING: TGtkWidgetSet.GetRawImageFromGdkWindow gdk_image_get failed');
    exit;
  end;
  try
    {$IFDEF VerboseRawImage}
    DebugLn('TGTKWidgetSet.GetRawImageMaskFromGdkBitmap A BytesPerLine=',dbgs(AnImage^.bpl),' theType=',dbgs(AnImage^.thetype),' depth=',dbgs(AnImage^.depth),' BitsPerpixel=',dbgs(AnImage^.bpp));
    DebugLn('RawImage=',RawImageDescriptionAsString(@RawImage));
    {$ENDIF}
  
    // See also GetWindowRawImageDescription
//    RawImage.Description.AlphaLineEnd:=rileDWordBoundary;
    case AnImage^.Depth of
    0..8:   RawImage.Description.AlphaLineEnd:=rileByteBoundary;
    9..32:  RawImage.Description.AlphaLineEnd:=rileDWordBoundary; //TODO MWE: Isn't there a word boundary (9..16) ?
    else    RawImage.Description.AlphaLineEnd:=rileQWordBoundary;
    end;
    RawImage.Description.AlphaBitsPerPixel:=AnImage^.bpp;

    // consistency checks
    if RawImage.Description.AlphaBitsPerPixel<>AnImage^.Depth then
      RaiseGDBException('RawImage.Description.AlphaBitsPerPixel<>AnImage^.Depth '+IntToStr(RawImage.Description.AlphaBitsPerPixel)+'<>'+IntToStr(AnImage^.Depth));

    RawImage.MaskSize:=AnImage^.bpl * AnImage^.Height;
    {$IFDEF VerboseRawImage}
    DebugLn('TGtkWidgetSet.GetRawImageFromGdkWindow G Width=',dbgs(AnImage^.Width),' Height=',dbgs(AnImage^.Height),' BitsPerPixel=',dbgs(RawImage.Description.AlphaBitsPerPixel),' bpl=',dbgs(AnImage^.bpl));
    {$ENDIF}
    if RawImage.MaskSize<>cardinal(AnImage^.bpl) * AnImage^.Height then
      RaiseGDBException('RawImage.MaskSize<>AnImage^.bpl*AnImage^.Height');

    // copy data
    ReAllocMem(RawImage.Mask,RawImage.MaskSize);
    if RawImage.MaskSize>0 then
      System.Move(AnImage^.Mem^,RawImage.Mask^,RawImage.MaskSize);

    {$IFDEF VerboseRawImage}
    DebugLn('TGtkWidgetSet.GetRawImageMaskFromGdkBitmap H ',
      ' Width=',dbgs(RawImage.Description.Width),
      ' Height=',dbgs(RawImage.Description.Height),
      ' AlphaBitsPerPixel=',dbgs(RawImage.Description.AlphaBitsPerPixel),
      ' MaskSize=',dbgs(RawImage.MaskSize));
    {$ENDIF}
  finally
    gdk_image_destroy(AnImage);
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
  fGC: PGDKGC;
  SrcDevContext, DestDevContext: TDeviceContext;
  SrcGDIBitmap: PGdiObject;
  TempPixmap, TempMaskPixmap: PGdkPixmap;
  NewClipMask: PGdkPixmap;
  SizeChange, ROpIsSpecial: Boolean;
  CopyingWholeSrc: Boolean;
  SrcWholeWidth, SrcWholeHeight: integer;
  DestWholeWidth, DestWholeHeight: integer;

  Procedure ResetClipping(DestGC : PGDKGC);
  begin
    ResetGCClipping(DestDC,DestGC);
    if (NewClipMask <> nil) then begin
      gdk_bitmap_unref(NewClipMask);
      NewClipMask:=nil;
    end;
  end;

  Function ScaleAndROP(DestGC: PGDKGC;
    Src: PGDKDrawable; SrcPixmap, SrcMaskPixmap: PGdkPixmap): Boolean;
  var
    Depth: Integer;
  begin
    {$IFDEF VerboseStretchCopyArea}
    
    DebugLn('ScaleAndROP START DestGC=',HexStr(Cardinal(DestGC),8),
      ' SrcPixmap=',HexStr(Cardinal(SrcPixmap),8),
      ' SrcMaskPixmap=',HexStr(Cardinal(SrcMaskPixmap),8));
    {$ENDIF}
    Result := False;

    if DestGC = nil
    then begin
      DebugLn('WARNING: [TGtkWidgetSet.StretchCopyArea] Uninitialized DestGC');
      exit;
    end;

    // copy the destination GC values into the temporary GC (fGC)
    GDK_GC_COPY(fGC, DestGC);

    // clear any previous clipping in the temporary GC (fGC)
    gdk_gc_set_clip_region(fGC,nil);
    gdk_gc_set_clip_rectangle(fGC,nil);

    if CopyingWholeSrc then ;

    if SizeChange then begin
      {$IFDEF VerboseStretchCopyArea}
      Depth:=gdk_visual_get_system^.Depth;
      DebugLn('ScaleAndROP Scaling buffer: ',Width,' x ',Height,' x ',Depth,' CopyingWholeSrc=',CopyingWholeSrc);
      {$ENDIF}
      // Scale the src part to a temporary pixmap with the size of the
      // destination rectangle
      Result := ScalePixmap(fGC,
                            SrcPixmap,XSrc,YSrc,SrcWidth,SrcHeight,
                            GDK_ColorMap_Get_System,
                            Width,Height,TempPixmap);
      if not Result then begin
        DebugLn('WARNING: ScaleAndROP ScalePixmap for pixmap failed');
        exit;
      end;
      // same for mask
      if SrcMaskPixmap<>nil then begin
        DebugLn('WARNING: ScaleAndROP  Scaling mask not yet implemented');
        {ColorMap:=gdk_colormap_new(gdk_visual_get_best_with_depth(1),2);
        Result := ScalePixmap(DestGC,
                              SrcMaskPixmap,XSrc,YSrc,SrcWidth,SrcHeight,
                              ColorMap,
                              Width,Height,TempMaskPixmap);
        gdk_colormap_unref(ColorMap);
        if not Result then begin
          DebugLn('WARNING: ScaleAndROP ScalePixmap for mask failed');
          exit;
        end;}
      end;

    end else if ROpIsSpecial then begin
      // no scaling, but special ROp

      Depth:=gdk_visual_get_system^.Depth;
      {$IFDEF VerboseStretchCopyArea}
      DebugLn('ScaleAndROP Creating rop buffer: ',Width,' x ',Height,' x ',Depth);
      {$ENDIF}
      TempPixmap := gdk_pixmap_new(nil,SrcWidth,SrcHeight,Depth);
      gdk_window_copy_area(TempPixmap, fGC, 0, 0,
         Src, XSrc, YSrc, SrcWidth, SrcHeight);
    end;

    // set raster operation in the destination GC
    SetGCRasterOperation(DestGC,ROP);

    Result:=true;
  end;

  Procedure ROPFillBuffer(DC : hDC);
  var
    OldCurrentBrush: PGdiObject;
    Brush : hBrush;
  begin
    if TempPixmap=nil then exit;
    if (ROp=WHITENESS) or (ROp=BLACKNESS) or (ROp=DSTINVERT) then begin
      {$IFDEF VerboseStretchCopyArea}
      DebugLn('ROPFillBuffer ROp=',ROp);
      {$ENDIF}
      with TDeviceContext(DC) do
      begin
        // Temporarily hold the old brush to
        // replace it with the given brush
        OldCurrentBrush := CurrentBrush;
        If ROP = WHITENESS then
          Brush := GetStockObject(WHITE_BRUSH)
        else
          Brush := GetStockObject(BLACK_BRUSH);
        CurrentBrush := PGdiObject(Brush);
        SelectedColors := dcscCustom;
        SelectGDKBrushProps(DC);

        If not CurrentBrush^.IsNullBrush then begin
          gdk_draw_rectangle(TempPixmap, GC, 1, 0, 0, Width, Height);
        end;
        // Restore current brush
        SelectedColors := dcscCustom;
        CurrentBrush := OldCurrentBrush;
      end;
    end;
  end;
  
  function SrcDevBitmapToDrawable: Boolean;
  var
    SrcPixmap, MaskPixmap: PGdkPixmap;
  begin
    Result:=true;
    {$IFDEF VerboseStretchCopyArea}
    DebugLn('SrcDevBitmapToDrawable Start');
    {$ENDIF}
    SrcGDIBitmap:=SrcDevContext.CurrentBitmap;
    if (SrcGDIBitmap=nil) then begin
      DebugLn('SrcDevBitmapToDrawable NOTE: SrcDevContext.CurrentBitmap=nil');
      exit;
    end;
    SrcPixmap:=SrcGDIBitmap^.GDIPixmapObject;
    MaskPixmap:=nil;
    if (Mask<>0) then
      MaskPixmap:=PGdiObject(Mask)^.GDIBitmapMaskObject;
    if MaskPixmap=nil then
      MaskPixmap:=SrcGDIBitmap^.GDIBitmapMaskObject;
    {$IFDEF VerboseStretchCopyArea}
    DebugLn('SrcDevBitmapToDrawable SrcPixmap=[',GetWindowDebugReport(SrcPixmap),']',
      ' MaskPixmap=[',GetWindowDebugReport(MaskPixmap),']');
    {$ENDIF}

    if (MaskPixmap=nil) and (not SizeChange) and (ROP=SRCCOPY)
    then begin
      // simply copy the area
      {$IFDEF VerboseStretchCopyArea}
      DebugLn('SrcDevBitmapToDrawable Simple copy');
      {$ENDIF}
      gdk_window_copy_area(DestDevContext.Drawable, DestDevContext.GC, X, Y,
        SrcPixmap, XSrc, YSrc, Width, Height);

      exit;
    end;


    // create a temporary graphic context for the scale and raster operations
    fGC := GDK_GC_New(DestDevContext.Drawable);

    // perform raster operation and scaling into Scale and fGC
    DestDevContext.SelectedColors := dcscCustom;
    If not ScaleAndROP(DestDevContext.GC, SrcDevContext.Drawable, SrcPixmap,
      MaskPixmap)
    then begin
      DebugLn('WARNING: SrcDevBitmapToDrawable: ScaleAndROP failed');
      exit;
    end;
    {$IFDEF VerboseStretchCopyArea}
    DebugLn('SrcDevBitmapToDrawable TempPixmap=',HexStr(Cardinal(TempPixmap),8),' TempMaskPixmap=',HexStr(Cardinal(TempMaskPixmap),8));
    {$ENDIF}
    if TempPixmap<>nil then begin
      SrcPixmap:=TempPixmap;
      XSrc:=0;
      YSrc:=0;
      SrcWidth:=Width;
      SrcHeight:=Height;
    end;
    if TempMaskPixmap<>nil then begin
      MaskPixmap:=TempMaskPixmap;
      XMask:=0;
      YMask:=0;
    end;

    GDK_GC_Unref(fGC);

    Case ROP of
      WHITENESS, BLACKNESS :
        ROPFillBuffer(DestDC);
    end;

    {$IFDEF VerboseStretchCopyArea}
    DebugLn('SrcDevBitmapToDrawable ',
      ' SrcPixmap=',HexStr(Cardinal(SrcPixmap),8),
      ' XSrc=',XSrc,' YSrc=',YSrc,' SrcWidth=',SrcWidth,' SrcHeight=',SrcHeight,
      ' MaskPixmap=',HexStr(Cardinal(MaskPixmap),8),
      ' XMask=',XMask,' YMask=',YMask,
      '');
    {$ENDIF}

    // set clipping mask for transparency
    MergeClipping(DestDevContext, DestDevContext.GC, X,Y,Width,Height,
                  MaskPixmap,XMask,YMask,
                  NewClipMask);

    // draw image
    {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
    gdk_window_copy_area(DestDevContext.Drawable, DestDevContext.GC, X, Y,
      SrcPixmap, XSrc, YSrc, SrcWidth, SrcHeight);
    {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
    // unset clipping mask for transparency
    ResetClipping(DestDevContext.GC);

    // restore raster operation to SRCCOPY
    GDK_GC_Set_Function(DestDevContext.GC, GDK_Copy);

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

  function ImageToImage: Boolean;
  begin
    DebugLn('WARNING: [TGtkWidgetSet.StretchCopyArea] ImageToImage unimplemented!');
    Result:=false;
  end;

  function ImageToDrawable: Boolean;
  begin
    DebugLn('WARNING: [TGtkWidgetSet.StretchCopyArea] ImageToDrawable unimplemented!');
    Result:=false;
  end;

  function ImageToBitmap: Boolean;
  begin
    DebugLn('WARNING: [TGtkWidgetSet.StretchCopyArea] ImageToBitmap unimplemented!');
    Result:=false;
  end;

  function PixmapToImage: Boolean;
  begin
    DebugLn('WARNING: [TGtkWidgetSet.StretchCopyArea] PixmapToImage unimplemented!');
    Result:=false;
  end;

  function PixmapToBitmap: Boolean;
  begin
    DebugLn('WARNING: [TGtkWidgetSet.StretchCopyArea] PixmapToBitmap unimplemented!');
    Result:=false;
  end;

  function BitmapToImage: Boolean;
  begin
    DebugLn('WARNING: [TGtkWidgetSet.StretchCopyArea] BitmapToImage unimplemented!');
    Result:=false;
  end;

  function BitmapToPixmap: Boolean;
  begin
    DebugLn('WARNING: [TGtkWidgetSet.StretchCopyArea] BitmapToPixmap unimplemented!');
    Result:=false;
  end;

  function Unsupported: Boolean;
  begin
    DebugLn('WARNING: [TGtkWidgetSet.StretchCopyArea] Destination and/or Source '
      + 'unsupported!!');
    Result:=false;
  end;

  //----------
  function NoDrawableToNoDrawable: Boolean;
  begin
    If (SrcDevContext.CurrentBitmap <> nil) and
      (DestDevContext.CurrentBitmap <> nil)
    then
      case SrcDevContext.CurrentBitmap^.GDIBitmapType of
      gbBitmap: case TDeviceContext(DestDC).CurrentBitmap^.GDIBitmapType of
                gbBitmap: Result:=DrawableToDrawable;
                gbPixmap: Result:=BitmapToPixmap;
                end;
      gbPixmap: case TDeviceContext(DestDC).CurrentBitmap^.GDIBitmapType of
                gbBitmap: Result:=PixmapToBitmap;
                gbPixmap: Result:=DrawableToDrawable;
                end;
      end
    else
      Result := Unsupported;
  end;

  function NoDrawableToDrawable: Boolean;
  begin
    If SrcDevContext.CurrentBitmap <> nil then
      case TDeviceContext(SrcDC).CurrentBitmap^.GDIBitmapType of
      gbBitmap: Result:=PixmapToDrawable;
      gbPixmap: Result:=PixmapToDrawable;
      end
    else
      Result := Unsupported;
  end;

  function DrawableToNoDrawable: Boolean;
  begin
    If DestDevContext.CurrentBitmap <> nil then
      case TDeviceContext(DestDC).CurrentBitmap^.GDIBitmapType of
      gbBitmap: Result:=Unsupported;
      gbPixmap: Result:=Unsupported;
      end
    else
      Result := Unsupported;
  end;
  
  procedure RaiseSrcDrawableNil;
  begin
    RaiseException('TGtkWidgetSet.StretchCopyArea SrcDC='+HexStr(Cardinal(SrcDevContext),8)+' Drawable=nil');
  end;

  procedure RaiseDestDrawableNil;
  begin
    RaiseException('TGtkWidgetSet.StretchCopyArea DestDC='+HexStr(Cardinal(DestDevContext),8)+' Drawable=nil');
  end;

var
  NewSrcWidth: Integer;
  NewSrcHeight: Integer;
  NewWidth: Integer;
  NewHeight: Integer;
  SrcDCOrigin: TPoint;
  DestDCOrigin: TPoint;
begin
  Result := IsValidDC(DestDC) and IsValidDC(SrcDC);
  {$IFDEF VerboseStretchCopyArea}
  DebugLn('StretchCopyArea Start ',Result);
  {$ENDIF}
  if not Result then exit;
  if (Width=0) or (Height=0) then exit;
  if (SrcWidth=0) or (SrcHeight=0) then exit;
  SizeChange:=(Width<>SrcWidth) or (Height<>SrcHeight);
  ROpIsSpecial:=(ROp<>SRCCOPY);
  SrcDevContext:=TDeviceContext(SrcDC);
  DestDevContext:=TDeviceContext(DestDC);

  with SrcDevContext do begin
    SrcDCOrigin:=GetDCOffset(TDeviceContext(SrcDC));
    Inc(XSrc,SrcDCOrigin.X);
    Inc(YSrc,SrcDCOrigin.Y);
    if Drawable=nil then RaiseSrcDrawableNil;
    gdk_window_get_size(PGdkWindow(Drawable),@SrcWholeWidth,@SrcWholeHeight);
  end;
  with DestDevContext do begin
    DestDCOrigin:=GetDCOffset(TDeviceContext(DestDC));
    Inc(X,DestDCOrigin.X);
    Inc(Y,DestDCOrigin.Y);
    if Drawable=nil then RaiseDestDrawableNil;
    gdk_window_get_size(PGdkWindow(Drawable),@DestWholeWidth,@DestWholeHeight);
  end;

  {$IFDEF VerboseStretchCopyArea}
  DebugLn('TGtkWidgetSet.StretchCopyArea BEFORE CLIPPING X=',X,' Y=',Y,' Width=',Width,' Height=',Height,
    ' XSrc=',XSrc,' YSrc=',YSrc,' SrcWidth=',SrcWidth,' SrcHeight=',SrcHeight,
    ' SrcDrawable=',HexStr(Cardinal(TDeviceContext(SrcDC).Drawable),8),
    ' SrcOrigin=',SrcDCOrigin.X,',',SrcDCOrigin.Y,
    ' DestDrawable=',HexStr(Cardinal(TDeviceContext(DestDC).Drawable),8),
    ' DestOrigin=',DestDCOrigin.X,',',DestDCOrigin.Y,
    ' Mask=',HexStr(Cardinal(Mask),8),' XMask=',XMask,' YMask=',YMask,
    ' SizeChange=',SizeChange,' ROpIsSpecial=',ROpIsSpecial,
    ' DestWhole=',DestWholeWidth,',',DestWholeHeight,
    ' SrcWhole=',SrcWholeWidth,',',SrcWholeHeight,
    '');
  {$ENDIF}

  if (X>=DestWholeWidth) or (Y>=DestWholeHeight) then exit;
  if (X+Width<=0) then exit;
  if (Y+Height<=0) then exit;
  if (XSrc>=SrcWholeWidth) or (YSrc>=SrcWholeHeight) then exit;
  if (XSrc+SrcWidth<=0) then exit;
  if (YSrc+SrcHeight<=0) then exit;

  // gdk does not allow copying areas, party laying out of bounds
  // -> clip

  // clip src to the left
  if (XSrc<0) then begin
    NewSrcWidth:=SrcWidth+XSrc;
    NewWidth:=((Width*NewSrcWidth) div SrcWidth);
    {$IFDEF VerboseStretchCopyArea}
    DebugLn('StretchCopyArea Cliping Src to left NewSrcWidth=',NewSrcWidth,' NewWidth=',NewWidth);
    {$ENDIF}
    if NewWidth=0 then exit;
    inc(X,Width-NewWidth);
    if (X>=DestWholeWidth) then exit;
    XSrc:=0;
    SrcWidth:=NewSrcWidth;
  end;

  // clip src to the top
  if (YSrc<0) then begin
    NewSrcHeight:=SrcHeight+YSrc;
    NewHeight:=((Height*NewSrcHeight) div SrcHeight);
    {$IFDEF VerboseStretchCopyArea}
    DebugLn('StretchCopyArea Cliping Src to top NewSrcHeight=',NewSrcHeight,' NewHeight=',NewHeight);
    {$ENDIF}
    if NewHeight=0 then exit;
    inc(Y,Height-NewHeight);
    if (Y>=DestWholeHeight) then exit;
    YSrc:=0;
    SrcHeight:=NewSrcHeight;
  end;

  // clip src to the right
  if (XSrc+SrcWidth>SrcWholeWidth) then begin
    NewSrcWidth:=SrcWholeWidth-XSrc;
    Width:=((Width*NewSrcWidth) div SrcWidth);
    {$IFDEF VerboseStretchCopyArea}
    DebugLn('StretchCopyArea Cliping Src to right NewSrcWidth=',NewSrcWidth,' NewWidth=',Width);
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
    DebugLn('StretchCopyArea Cliping Src to bottom NewSrcHeight=',NewSrcHeight,' NewHeight=',Height);
    {$ENDIF}
    if (Height=0) then exit;
    if (Y+Height<=0) then exit;
    SrcHeight:=NewSrcHeight;
  end;

  CopyingWholeSrc:=(XSrc=0) and (YSrc=0)
                   and (SrcWholeWidth=SrcWidth) and (SrcWholeHeight=SrcHeight);


  if Mask=0 then begin
    XMask:=XSrc;
    YMask:=YSrc;
  end;

  // mark temporary scaling/rop buffers as uninitialized
  TempPixmap:=nil;
  TempMaskPixmap:=nil;

  {$IFDEF VerboseStretchCopyArea}
  write('TGtkWidgetSet.StretchCopyArea AFTER CLIPPING X=',X,' Y=',Y,' Width=',Width,' Height=',Height,
    ' XSrc=',XSrc,' YSrc=',YSrc,' SrcWidth=',SrcWidth,' SrcHeight=',SrcHeight,
    ' SrcDrawable=',HexStr(Cardinal(TDeviceContext(SrcDC).Drawable),8),
    ' DestDrawable=',HexStr(Cardinal(TDeviceContext(DestDC).Drawable),8),
    ' Mask=',HexStr(Cardinal(Mask),8),' XMask=',XMask,' YMask=',YMask,
    ' SizeChange=',SizeChange,' ROpIsSpecial=',ROpIsSpecial,
    ' CopyingWholeSrc=',CopyingWholeSrc);
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

  If TDeviceContext(SrcDC).Drawable = nil then begin
    If TDeviceContext(DestDC).Drawable = nil then
      Result := NoDrawableToNoDrawable
    else
      Result := NoDrawableToDrawable;
  end
  else begin
    If TDeviceContext(DestDC).Drawable = nil then
      Result := DrawableToNoDrawable
    else
      Result := DrawableToDrawable;
  end;
  
  if TempPixmap<>nil then
    gdk_pixmap_unref(TempPixmap);
  if TempMaskPixmap<>nil then
    gdk_pixmap_unref(TempMaskPixmap);
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
      gtk_list_set_selection_mode(PGtkList(
        GetWidgetInfo(Widget, True)^.CoreWidget),
        SelectionMode);

    csCListBox:
      gtk_clist_set_selection_mode(PGtkCList(
        GetWidgetInfo(Widget, True)^.CoreWidget),
          SelectionMode);

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
begin
  // change cursor
  if AComponent is TWinControl then
    gtkproc.SetCursor(TWinControl(AComponent), TCursor(cardinal(nil)));
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
  Widget: PGtkWidget;
begin
  //DebugLn('[TGtkWidgetSet.ResizeChild] START ',TControl(Sender).Name,':',Sender.Classname,' Left=',Left,' Top=',Top,' Width=',Width,' Height=',Height);
  Assert(false, (Format('trace:  [TGtkWidgetSet.ResizeChild] %s --> Resize', [Sender.ClassNAme])));

  if Sender is TWinControl then begin
    if TWinControl(Sender).HandleAllocated then begin
      Widget := pgtkWidget(TWinControl(Sender).Handle);
      SetResizeRequest(Widget);
      //if (Sender is TCustomForm) then
      //if AnsiCompareText(Sender.ClassName,'TScrollBar')=0 then
      //  DebugLn(' FFF ResizeChild ',Sender.ClassName,' ',Left,',',Top,',',Width,',',Height);
    end;
  end;
  //DebugLn('[TGtkWidgetSet.ResizeChild] END ',Sender.Classname,' Left=',Left,' Top=',Top,' Width=',Width,' Height=',Height);
end;

{------------------------------------------------------------------------------
  Function: TGtkWidgetSet.SetCallback
  Params: AMsg - message for which to set a callback
          AGTKObject - object to which callback will be send
          ALCLObject - for compatebility reasons provided, will be used when
                       AGTKObject = nil
  Returns:  nothing

  Applies a Message to the sender
 ------------------------------------------------------------------------------}
//TODO: remove ALCLObject when creation splitup is finished
procedure TGtkWidgetSet.SetCallback(const AMsg: LongInt; const AGTKObject: PGTKObject; const ALCLObject: TObject);

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
    ConnectSenderSignal(AnObject,
      'key-press-event', @GTKKeyUpDown, GDK_KEY_PRESS_MASK);
    ConnectSenderSignalAfter(AnObject,
      'key-press-event', @GTKKeyUpDownAfter, GDK_KEY_PRESS_MASK);
    ConnectSenderSignal(AnObject,
      'key-release-event', @GTKKeyUpDown, GDK_KEY_RELEASE_MASK);
    ConnectSenderSignalAfter(AnObject,
      'key-release-event', @GTKKeyUpDownAfter, GDK_KEY_RELEASE_MASK);
  end;

var
  gObject, gFixed, gCore, Scroll: PGTKObject;
begin
  if AGTKObject = nil
  then gObject := ObjectToGTKObject(ALCLObject)
  else gObject := AGTKObject;
  if gObject = nil then Exit;

  // gFixed is the widget with the client area (e.g. TGroupBox, TCustomForm have this)
  gFixed := PGTKObject(GetFixedWidget(gObject));
  if gFixed = nil then gFixed := gObject;

  // gCore is the main widget (e.g. TListView has this)
  gCore:= PGtkObject(GetWidgetInfo(gObject, True)^.CoreWidget);

  case AMsg of
    LM_SHOWWINDOW :
    begin
      ConnectSenderSignal(gObject, 'show', @gtkshowCB);
      ConnectSenderSignal(gObject, 'hide', @gtkhideCB);
    end;

    LM_DESTROY :
    begin
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
      ConnectSenderSignal(gObject, 'day-selected', @gtkdaychanged);
      ConnectSenderSignal(gObject, 'day-selected-double-click', @gtkdaychanged);
    end;

    LM_PAINT :
    begin
      ConnectSenderSignalAfter(gFixed, 'expose-event', @GTKExposeEventAfter);
      {$Ifdef GTK1}
      ConnectSenderSignalAfter(gFixed, 'draw', @GTKDrawAfter);
      {$EndIf}
      ConnectSenderSignal(gFixed,'style-set', @GTKStyleChanged);
    end;

    LM_FOCUS :
    begin
      if (ALCLObject is TCustomComboBox) then begin
        ConnectFocusEvents(PgtkObject(PgtkCombo(gObject)^.entry));
        ConnectFocusEvents(PgtkObject(PgtkCombo(gObject)^.list));
      end else begin
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
      if (ALCLObject is TCustomComboBox) then begin
        ConnectKeyPressReleaseEvents(PgtkObject(PgtkCombo(gObject)^.entry));
      end
      else if (ALCLObject is TCustomForm) then begin
        ConnectKeyPressReleaseEvents(gObject);
      end;
      ConnectKeyPressReleaseEvents(gCore);
    end;

    LM_MONTHCHANGED:  //calendar
    Begin
      ConnectSenderSignal(gObject, 'month-changed', @gtkmonthchanged);
      ConnectSenderSignal(gObject, 'prev-month', @gtkmonthchanged);
      ConnectSenderSignal(gObject, 'next-month', @gtkmonthchanged);
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
      else begin
        ConnectSenderSignal(gFixed, 'motion-notify-event', @GTKMotionNotify,
                      GDK_POINTER_MOTION_MASK);
        ConnectSenderSignalAfter(gFixed, 'motion-notify-event',
                      @GTKMotionNotifyAfter, GDK_POINTER_MOTION_MASK);
      end;
    end;

    LM_LBUTTONDOWN,
    LM_RBUTTONDOWN,
    LM_MBUTTONDOWN,
    LM_MOUSEWHEEL :
    begin
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
      else begin
        ConnectSenderSignal(gFixed, 'button-press-event', @gtkMouseBtnPress,
                            GDK_BUTTON_PRESS_MASK);
        ConnectSenderSignalAfter(gFixed, 'button-press-event',
                                 @gtkMouseBtnPressAfter, GDK_BUTTON_PRESS_MASK);
      end;
    end;

    LM_LBUTTONUP,
    LM_RBUTTONUP,
    LM_MBUTTONUP:
    begin
      if (ALCLObject is TCustomComboBox) then
      Begin
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
      else begin
        ConnectSenderSignal(gFixed, 'button-release-event', @gtkMouseBtnRelease,
                      GDK_BUTTON_RELEASE_MASK);
        ConnectSenderSignalAfter(gFixed, 'button-release-event',
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
      //if ALCLObject is TCustomListView
      //then begin
      //  ConnectSenderSignal(gObject, 'scroll-horizontal', @gtkLVHScroll);
      //end
      //else begin
      If ALCLObject is TScrollBar then
        ConnectSenderSignal(
                PGTKObject(PgtkhScrollBar(gObject)^.Scrollbar.Range.Adjustment),
                           'value-changed', @GTKHScrollCB)
      else If ALCLObject is TScrollBox then begin
        Scroll := gtk_object_get_data(gObject, odnScrollArea);
        ConnectSenderSignal(PGTKObject(gtk_scrolled_window_get_hadjustment(
          PGTKScrolledWindow(Scroll))), 'value-changed', @GTKHScrollCB);
      end
      else
        ConnectSenderSignal(PGTKObject(gtk_scrolled_window_get_hadjustment(
          PGTKScrolledWindow(gObject))), 'value-changed', @GTKHScrollCB);
      //end;
    end;

    LM_VSCROLL:
    begin
      //if ALCLObject is TCustomListView
      //then begin
      //  ConnectSenderSignal(gObject, 'scroll-vertical', @gtkLVVScroll);
      //end
      //else begin
      If ALCLObject is TScrollBar then
        ConnectSenderSignal(
                PGTKObject(PgtkhScrollBar(gObject)^.Scrollbar.Range.Adjustment),
                           'value-changed', @GTKVScrollCB)
      else If ALCLObject is TScrollBox then begin
        Scroll := gtk_object_get_data(gObject, odnScrollArea);
        ConnectSenderSignal(PGTKObject(gtk_scrolled_window_get_vadjustment(
          PGTKScrolledWindow(Scroll))), 'value-changed', @GTKVScrollCB);
      end
      else
        ConnectSenderSignal(PGTKObject(gtk_scrolled_window_get_vadjustment(
          PGTKScrolledWindow(gObject))), 'value-changed', @GTKVScrollCB);
      //end;
    end;

    LM_YEARCHANGED :  //calendar
    Begin
      ConnectSenderSignal(gObject, 'prev-year', @gtkyearchanged);
      ConnectSenderSignal(gObject, 'next-year', @gtkyearchanged);
    end;

    // Listview & Header control

    //HDN_BEGINTRACK
    //HDN_DIVIDERDBLCLICK

    HDN_ENDTRACK,
    HDN_TRACK:
    begin
      ConnectSenderSignal(gObject, 'resize-column', @gtkLVResizeColumn);
      ConnectSenderSignal(gObject, 'abort-column-resize',
                          @gtkLVAbortColumnResize);
    end;

    HDN_ITEMCHANGED,
    HDN_ITEMCHANGING:
    begin
      ConnectSenderSignal(gObject, 'resize-column', @gtkLVResizeColumn);
    end;

//    HDN_ITEMDBLCLICK
    HDN_ITEMCLICK,
    LVN_COLUMNCLICK:
    begin
      ConnectSenderSignal(gCore, 'click-column', @gtkLVClickColumn);
    end;

//    LVN_DELETEALLITEMS,
    LVN_DELETEITEM,
    LVN_INSERTITEM:
    begin
      ConnectSenderSignal(gCore, 'row-move', @gtkLVRowMove);
    end;

    LVN_ITEMCHANGED,
    LVN_ITEMCHANGING:
    begin
      ConnectSenderSignal(gCore, 'select-row', @gtkLVSelectRow);
      ConnectSenderSignal(gCore, 'unselect-row', @gtkLVUnSelectRow);
      ConnectSenderSignal(gCore, 'toggle-focus-row', @gtkLVToggleFocusRow);
      ConnectSenderSignal(gCore, 'select-all', @gtkLVSelectAll);
      ConnectSenderSignal(gCore, 'unselect-all', @gtkLVUnSelectAll);
      ConnectSenderSignal(gCore, 'end-selection', @gtkLVEndSelection);
    end;

    LM_COMMAND:
    begin
      if ALCLObject is TCustomComboBox then begin
        ConnectSenderSignalAfter(PgtkObject(PgtkCombo(gObject)^.popwin),
          'show', @gtkComboBoxShowCB);
        ConnectSenderSignalAfter(PgtkObject(PgtkCombo(gObject)^.popwin),
          'hide', @gtkComboBoxHideCB);
      end;
    end;
    
    LM_SelChange:
    begin
      if ALCLObject is TCustomListBox then begin
        ConnectSenderSignalAfter(PgtkObject(gCore),
          'selection_changed', @gtkListBoxSelectionChangedCB);
      end;
    end;

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


{------------------------------------------------------------------------------
  Function: TGtkWidgetSet.RemoveCallBacks
  Params:   Widget
  Returns:  nothing

  Removes Call Back Signals from the Widget
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.RemoveCallbacks(Widget: PGtkWidget);
var
  MainWidget, ClientWidget, ImplWidget: PGtkWidget;
  WinWidgetInfo: PWinWidgetInfo;
begin
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
  Handle := hwnd(ObjectToGtkObject(Sender));
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

  // clipboard widget

  // Remove control accelerators - has to be done due to GTK+ bug?
  {$IFNDef GTK2}
  //DebugLn('TGtkWidgetSet.DestroyLCLComponent B ',TWinControl(Sender).Name,':',TWinControl(Sender).ClassName);
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
    SetComboBoxText(PGtkCombo(Widget),nil);
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
    DestroyCaret(HDC(Widget));

  // remove pending size messages
  UnsetResizeRequest(Widget);
  FWidgetsResized.Remove(Widget);
  if FixWidget<>Widget then
    FFixWidgetsResized.Remove(FixWidget);

  // destroy the widget
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
  HistoryList: TList; // list of THistoryListEntry
  AHistoryEntry: PFileSelHistoryEntry;
  i: integer;
  s: string;
  HBox, LabelWidget, HistoryPullDownWidget,
  MenuWidget, MenuItemWidget: PGtkWidget;
begin
  if OpenDialog.HistoryList.Count>0 then begin

    // create the HistoryList where the current state of the history is stored
    HistoryList:=TList.Create;
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
    gtk_box_pack_start(GTK_BOX(GTK_FILE_SELECTION(SelWidget)^.main_vbox),
                       HBox,false,false,0);

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
    gtk_widget_show(HBox);

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
  Function: ExtractFilterList
  Params: const Filter: string; var FilterIndex: integer;
          var FilterList: TStringList
  Returns: -

  Converts a Delphi file filter of the form
  'description1|mask1|description2|mask2|...'
  into a TList of PFileSelFilterEntry(s).
  Multi masks:
    - multi masks like '*.pas;*.pp' are converted into multiple entries.
    - if the masks are found in the description they are adjusted
    - if the mask is not included in the description it will be concatenated
    For example:
      'Pascal files (*.pas;*.pp)|*.pas;*.lpr;*.pp;
      is converted to three filter entries:
        'Pascal files (*.pas)' + '*.pas'
        'Pascal files (*.pp)'  + '*.pp'
        'Pascal files (*.lpr)' + '*.lpr'
 ------------------------------------------------------------------------------}
procedure ExtractFilterList(const Filter: string; var FilterList: TList);
var
  Masks: TStringList;
  CurFilterIndex: integer;

  procedure ExtractMasks(const MultiMask: string);
  var CurMaskStart, CurMaskEnd: integer;
    s: string;
  begin
    if Masks=nil then
      Masks:=TStringList.Create
    else
      Masks.Clear;
    CurMaskStart:=1;
    while CurMaskStart<=length(MultiMask) do begin
      CurMaskEnd:=CurMaskStart;
      while (CurMaskEnd<=length(MultiMask)) and (MultiMask[CurMaskEnd]<>';') do
        inc(CurMaskEnd);
      s:=Trim(copy(MultiMask,CurMaskStart,CurMaskEnd-CurMaskStart));
      if s='*.*' then s:='';
      Masks.Add(s);
      CurMaskStart:=CurMaskEnd+1;
    end;
  end;

  procedure AddEntry(const Desc, Mask: string);
  var NewFilterEntry: PFileSelFilterEntry;
  begin
    New(NewFilterEntry);
    NewFilterEntry^.Description:= StrAlloc(length(Desc)+1);
    StrPCopy(NewFilterEntry^.Description, Desc);
    NewFilterEntry^.Mask:= StrAlloc(length(Mask)+1);
    StrPCopy(NewFilterEntry^.Mask, Mask);
    NewFilterEntry^.FilterIndex:=CurFilterIndex;
    FilterList.Add(NewFilterEntry);
  end;

  // remove all but one masks from description string
  function RemoveOtherMasks(const Desc: string; MaskIndex: integer): string;
  var i, StartPos, EndPos: integer;
  begin
    Result:=Desc;
    for i:=0 to Masks.Count-1 do begin
      if i=MaskIndex then continue;
      StartPos:=Pos(Masks[i],Result);
      EndPos:=StartPos+length(Masks[i]);
      if StartPos<1 then continue;
      while (StartPos>1) and (Result[StartPos-1] in [' ',#9,';']) do
        dec(StartPos);
      while (EndPos<=length(Result)) and (Result[EndPos] in [' ',#9]) do
        inc(EndPos);
      if (StartPos>1) and (Result[StartPos-1]='(')
      and (EndPos<=length(Result)) then begin
        if (Result[EndPos]=')') then begin
          dec(StartPos);
          inc(EndPos);
        end else if Result[EndPos]=';' then begin
          inc(EndPos);
        end;
      end;
      System.Delete(Result,StartPos,EndPos-StartPos);
    end;
  end;

  procedure AddEntries(const Desc: string; MultiMask: string);
  var i: integer;
    CurDesc: string;
  begin
    ExtractMasks(MultiMask);
    for i:=0 to Masks.Count-1 do begin
      CurDesc:=RemoveOtherMasks(Desc,i);
      if (Masks.Count>1) and (Pos(Masks[i],CurDesc)<1) then begin
        if (CurDesc='') or (CurDesc[length(CurDesc)]<>' ') then
          CurDesc:=CurDesc+' ';
        CurDesc:=CurDesc+'('+Masks[i]+')';
      end;
      AddEntry(CurDesc,Masks[i]);
    end;
    inc(CurFilterIndex);
  end;

var
  CurDescStart, CurDescEnd, CurMultiMaskStart, CurMultiMaskEnd: integer;
  CurDesc, CurMultiMask: string;
begin
  FilterList:=TList.Create;
  Masks:=nil;
  CurFilterIndex:=0;
  CurDescStart:=1;
  while CurDescStart<=length(Filter) do begin
    // extract next filter description
    CurDescEnd:=CurDescStart;
    while (CurDescEnd<=length(Filter)) and (Filter[CurDescEnd]<>'|') do
      inc(CurDescEnd);
    CurDesc:=copy(Filter,CurDescStart,CurDescEnd-CurDescStart);
    // extract next filter multi mask
    CurMultiMaskStart:=CurDescEnd+1;
    CurMultiMaskEnd:=CurMultiMaskStart;
    while (CurMultiMaskEnd<=length(Filter)) and (Filter[CurMultiMaskEnd]<>'|') do
      inc(CurMultiMaskEnd);
    CurMultiMask:=copy(Filter,CurMultiMaskStart,CurMultiMaskEnd-CurMultiMaskStart);
    if CurDesc='' then CurDesc:=CurMultiMask;
    // add filter(s)
    if (CurMultiMask<>'') or (CurDesc<>'') then
      AddEntries(CurDesc,CurMultiMask);
    // next filter
    CurDescStart:=CurMultiMaskEnd+1;
  end;
  Masks.Free;
end;

{------------------------------------------------------------------------------
  Function: TGtkWidgetSet.CreateOpenDialogFilter
  Params: OpenDialog: TOpenDialog; SelWidget: PGtkWidget
  Returns: -

  Adds a Filter pulldown to a gtk file selection dialog.
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.CreateOpenDialogFilter(OpenDialog: TOpenDialog;
  SelWidget: PGtkWidget);
var
  FilterList: TList;
  HBox, LabelWidget, FilterPullDownWidget,
  MenuWidget, MenuItemWidget: PGtkWidget;
  i, CurMask: integer;
  s: String;
begin
  ExtractFilterList(OpenDialog.Filter,FilterList);
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
  if FilterList.Count>0 then begin
    i:=0;
    CurMask:=0;
    while (i<FilterList.Count) do begin
      if PFileSelFilterEntry(FilterList[i])^.FilterIndex=OpenDialog.FilterIndex
      then begin
        CurMask:=i;
        break;
      end;
      inc(i);
    end;
    gtk_file_selection_complete(GTK_FILE_SELECTION(SelWidget),
         PFileSelFilterEntry(FilterList[CurMask])^.Mask);
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
begin
  FileSelWidget:=GTK_FILE_SELECTION(SelWidget);

  // Multiselection
  if ofAllowMultiSelect in OpenDialog.Options then
  begin
    LastFileSelectRow := -1;
    g_signal_connect(gtk_object(FileSelWidget^.file_list),
      'select-row',
      gtk_signal_func(@gtkOpenDialogRowSelectCB), OpenDialog);
    gtk_clist_set_selection_mode(
      PGtkCList(FileSelWidget^.file_list),GTK_SELECTION_MULTIPLE);
  end;

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
    SetLCLObject(FileSelWidget^.file_list,OpenDialog);
    g_signal_connect(gtk_object(FileSelWidget^.file_list),
      'select-row', gtk_signal_func(@GTKDialogSelectRowCB), OpenDialog);
  end;

  // History List - a frame with an option menu
  CreateOpenDialogHistory(OpenDialog,SelWidget);

  // Filter - a frame with an option menu
  CreateOpenDialogFilter(OpenDialog,SelWidget);

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
  if IsFontNameXLogicalFontDesc(FontDialog.Name) then
    gtk_font_selection_dialog_set_font_name(PGtkFontSelectionDialog(SelWidget),
      PChar(FontDialog.Name));

  InitializeCommonDialog(TCommonDialog(FontDialog),SelWidget);
end;

{-------------------------------------------------------------------------------
  function TGtkWidgetSet.CreateComboBox(ComboBoxObject: TObject): Pointer;
-------------------------------------------------------------------------------}
function TGtkWidgetSet.CreateComboBox(ComboBoxObject: TObject): Pointer;
var
  Widget: PGtkCombo;
  ItemList: TGtkListStringList;
  ComboBox: TComboBox;
begin
  ComboBox:=TComboBox(ComboBoxObject);
  Result:= gtk_combo_new();
  Widget:= PGTKCombo(Result);

  SetMainWidget(Result, Widget^.entry);

  gtk_combo_disable_activate(Widget);
  gtk_combo_set_case_sensitive(Widget, GdkTrue);

  // Items
  ItemList:= TGtkListStringList.Create(PGtkList(Widget^.List),ComboBox,False);
  gtk_object_set_data(PGtkObject(Widget), 'LCLList', ItemList);
  ItemList.Assign(ComboBox.Items);
  ItemList.Sorted:= ComboBox.Sorted;

  // ItemIndex
  if ComboBox.ItemIndex >= 0 then
    gtk_list_select_item(PGtkList(Widget^.list), ComboBox.ItemIndex);

  // MaxLength
  gtk_entry_set_max_length(PGtkEntry(Widget^.entry),guint16(ComboBox.MaxLength));

  // Text
  SetComboBoxText(Widget, PChar(ComboBox.Text));
end;


procedure TGtkWidgetSet.FinishComponentCreate(const ALCLObject: TObject; const AGTKObject: Pointer; const ASetupProps : Boolean);
begin
  // MWE: next will be obsoleted by WinWidgetInfo
  if AGTKObject <> nil then
  Begin
    SetLCLObject(AGTKObject, ALCLObject);
    gtk_object_set_data(AGTKObject, 'Style',GtkNil);
    gtk_object_set_data(AGTKObject, 'ExStyle',GtkNil);
  end;
  //--------------------------

  // in the new (compatebility) situation setting the handle should not be needed
  // however lazarus fails to start, so I'm enabling it for now
  if (ALCLObject is TWinControl) then
  begin
    TWinControl(ALCLObject).Handle := THandle(AGTKObject);
    if AGTKObject <> nil then begin
      gtk_object_set_data(AGTKObject, 'Sender', ALCLObject);
      SetResizeRequest(AGTKObject);
    end;
  end
  else
  if (ALCLObject is TMenuItem) then
    TMenuItem(ALCLObject).Handle := HMenu(AGTKObject)
  else
  if (ALCLObject is TMenu) then
    TMenu(ALCLObject).Items.Handle := HMenu(AGTKObject)
  else
  if (ALCLObject is TCommonDialog) then
    TCommonDialog(ALCLObject).Handle:= THandle(AGTKObject);

  Set_RC_Name(ALCLObject, AGTKObject);

  if ASetupProps then 
    { TODO: call this in CreateHandle when converted: SetProperties(ALCLObject) };

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
    gtk_object_set_data(PGTKObject(Adjustment), 'ScrollBar',
                        PGTKScrolledWindow(Result)^.VScrollBar);
    Step_Increment := 1;
  end;

  Adjustment :=
    gtk_scrolled_window_get_hadjustment(PGTKScrolledWindow(Result));
  if Adjustment <> nil
  then with Adjustment^ do
  begin
    gtk_object_set_data(PGTKObject(Adjustment), 'ScrollBar',
                        PGTKScrolledWindow(Result)^.HScrollBar);
    Step_Increment := 1;
  end;
end;

function TGtkWidgetSet.CreateForm(ACustomForm: TCustomForm): PGtkWidget;
var
  Box: Pointer;
  ABorderStyle: TFormBorderStyle;
  PCaption: PChar;
  WindowType: TGtkWindowType;
begin
  if ACustomForm.Parent=nil then begin
    if csDesigning in ACustomForm.ComponentState then
      ABorderStyle:=bsSizeable
    else
      ABorderStyle:=ACustomForm.BorderStyle;
  end else
    ABorderStyle:=bsNone;
  WindowType:=FormStyleMap[ABorderStyle];
  if (ABorderStyle=bsNone) and (ACustomForm.FormStyle in fsAllStayOnTop)
  and (not (csDesigning in ACustomForm.ComponentState)) then begin
    WindowType:=GTK_WINDOW_POPUP;
  end;

  if ACustomForm.Parent=nil then begin
    // create a floating form
    Result := gtk_window_new(WindowType);
    
    gtk_window_set_policy(GTK_WINDOW(Result), FormResizableMap[ABorderStyle],
      FormResizableMap[ABorderStyle], 0);
    PCaption:=PChar(ACustomForm.Caption);
    if PCaption=nil then PCaption:=#0;
    gtk_window_set_title(pGtkWindow(Result), PCaption);

    // the clipboard needs a widget
    if ClipboardWidget=nil then
      SetClipboardWidget(Result);

    //drag icons
    if Drag_Icon = nil then begin
      {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
      Drag_Icon := gdk_pixmap_colormap_create_from_xpm_d (nil,
                           gtk_widget_get_colormap (Result), Drag_Mask,
                           nil, @IMGDrag_Icon);
      {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
    end;
  end else begin
    // create a form as child control
    Result := gtk_hbox_new(false,0);
  end;

  Box := CreateFormContents(ACustomForm,Result);
  gtk_container_add(PGtkContainer(Result), Box);

  {$IfDef GTK2}
  //so we can double buffer ourselves, eg, the Form Designer
  gtk_widget_set_double_buffered(Box, False);
  {$EndIf}
  gtk_widget_show(Box);

  // main menu
  if (ACustomForm.Menu<>nil)
  and (ACustomForm.Menu.HandleAllocated) then begin
    gtk_box_pack_start(Box, PGtkWidget(ACustomForm.Menu.Handle),False,False,0);
  end;
end;

function TGtkWidgetSet.CreateListView(ListViewObject: TObject): PGtkWidget;
{$IFdef GTK2}
begin
  Result:=gtk_scrolled_window_new(nil, nil);//create something just in case
  gtk_widget_show(result);
  DebugLn('TODO: TGtkWidgetSet.CreateListView');
end;
{$Else}
var
  MainWidget: PGtkWidget;
  i: Integer;
  CListWidget: PGtkCList;
  ImpWidget: PGtkWidget;
  RealColumnCnt: Integer;
  Titles: PPGChar;
begin
  MainWidget:= gtk_scrolled_window_new(nil, nil);

  with TListView(ListViewObject)
  do begin
    RealColumnCnt:=Columns.Count;
    if RealColumnCnt<1 then RealColumnCnt:=1;
    CListWidget:=PGtkCList(gtk_clist_new(RealColumnCnt));
    gtk_clist_set_shadow_type(CListWidget,GTK_SHADOW_IN);
    gtk_clist_column_titles_passive (CListWidget);

    // add items (the item properties are set via LM_SETPROPERTIES)
    GetMem(Titles,SizeOf(PGChar)*CListWidget^.columns);
    for i:=0 to CListWidget^.columns-1 do
      Titles[i]:=nil;
    for i:=0 to Items.Count-1 do begin
      if Items[i].Caption<>'' then
        Titles[0] := PChar(Items[i].Caption)
      else
        Titles[0] := #0;
      gtk_clist_append(CListWidget,Titles);
    end;
    FreeMem(Titles);

    // set columns properties
    for i := 0 to Columns.Count - 1 do begin
      with Columns[i] do begin
        // set title
        gtk_clist_set_column_title(CListWidget,i, PChar(Caption));
        //set column alignment
        gtk_clist_set_column_justification(CListWidget,i,
                 aGTKJUSTIFICATION[Alignment]);
        //set width
        if Width>0 then
          gtk_clist_set_column_width(CListWidget,i,Width);
        //set auto sizing
        gtk_clist_set_column_auto_resize(CListWidget,i, AutoSize);
        //set Visible
        gtk_clist_set_column_visibility(CListWidget,i, Visible);
        // set MinWidth
        if MinWidth>0 then
          gtk_clist_set_column_min_width(CListWidget, i, MinWidth);
        // set MaxWidth
        if (MaxWidth>=MinWidth) and (MaxWidth>0) then
          gtk_clist_set_column_max_width(CListWidget, i, MaxWidth);
      end;
    end;

  end;
  gtk_clist_column_titles_passive (CListWidget);

  ImpWidget:=PGtkWidget(CListWidget);
  gtk_container_add(GTK_CONTAINER(MainWidget),ImpWidget);
  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(MainWidget)^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(MainWidget)^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(PGtkScrolledWindow(MainWidget),
                                 GTK_POLICY_AUTOMATIC,
                                 GTK_POLICY_AUTOMATIC);
  gtk_container_set_focus_vadjustment(PGtkContainer(CListWidget),
           gtk_scrolled_window_get_vadjustment(PGtkScrolledWindow(MainWidget)));
  gtk_container_set_focus_hadjustment(PGtkContainer(CListWidget),
           gtk_scrolled_window_get_hadjustment(PGtkScrolledWindow(MainWidget)));
  gtk_widget_show_all(ImpWidget);
  gtk_widget_show(MainWidget);

  SetMainWidget(MainWidget, ImpWidget);
  GetWidgetInfo(MainWidget, True)^.CoreWidget := ImpWidget;

  Result:=MainWidget;
end;
{$EndIF}
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
  function TGtkWidgetSet.CreatePairSplitter(PairSplitterObject: TObject
    ): PGtkWidget;

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
  procedure TGtkWidgetSet.UpdateStatusBarPanels(StatusBar: TObject;
    StatusBarWidget: PGtkWidget);

  Update the widget(s) of a TStatusBar.
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.OldUpdateStatusBarPanels(StatusBar: TObject;
  StatusBarWidget: PGtkWidget);
var
  AStatusBar: TStatusBar;
  HBox: PGtkWidget;
  CurPanelCount: integer;
  NewPanelCount: Integer;
  CurStatusPanelWidget: PGtkWidget;
  ListItem: PGList;
  i: Integer;
  ExpandItem: boolean;
begin
  //DebugLn('TGtkWidgetSet.UpdateStatusBarPanels ',HexStr(Cardinal(StatusBar),8));

  AStatusBar:=StatusBar as TStatusBar;
  HBox:=PGtkWidget(StatusBarWidget);
  if (not GtkWidgetIsA(StatusBarWidget,GTK_HBOX_GET_TYPE)) then
    RaiseGDBException('');

  // create needed panels
  CurPanelCount:=integer(g_list_length(PGtkBox(HBox)^.children));
  if AStatusBar.SimplePanel or (AStatusBar.Panels.Count<1) then
    NewPanelCount:=1
  else
    NewPanelCount:=AStatusBar.Panels.Count;
  while CurPanelCount<NewPanelCount do begin
    CurStatusPanelWidget:=CreateStatusBarPanel(StatusBar,CurPanelCount);
    ExpandItem:=(CurPanelCount=NewPanelCount-1);
    gtk_box_pack_start(PGtkBox(HBox),CurStatusPanelWidget,
                       ExpandItem,ExpandItem,0);
    inc(CurPanelCount);
  end;

  // remove unneeded panels
  while CurPanelCount>NewPanelCount do begin
    CurStatusPanelWidget:=PGtkBoxChild(
              g_list_nth_data(PGtkBox(HBox)^.children,CurPanelCount-1))^.Widget;
    DestroyConnectedWidget(CurStatusPanelWidget,true);
    dec(CurPanelCount);
  end;

  // check new panel count
  CurPanelCount:=integer(g_list_length(PGtkBox(HBox)^.children));
  //DebugLn('TGtkWidgetSet.UpdateStatusBarPanels B ',HexStr(Cardinal(StatusBar),8),' NewPanelCount=',NewPanelCount,' CurPanelCount=',CurPanelCount);
  if CurPanelCount<>NewPanelCount then
    RaiseGDBException('');

  // set panel properties
  ListItem:=PGTKBox(HBox)^.children;
  i:=0;
  while ListItem<>nil do begin
    CurStatusPanelWidget:=PGtkBoxChild(PGTKWidget(ListItem^.data))^.widget;
    ExpandItem:=(ListItem^.next=nil);
    gtk_box_set_child_packing(PGtkBox(HBox),CurStatusPanelWidget,
      ExpandItem,ExpandItem,0,GTK_PACK_START);
    UpdateStatusBarPanel(StatusBar,i,CurStatusPanelWidget);
    inc(i);
    ListItem:=ListItem^.next;
  end;
end;

{------------------------------------------------------------------------------
  procedure TGtkWidgetSet.UpdateStatusBarPanel(StatusBar: TObject; Index: integer;
    StatusPanelWidget: PGtkWidget);

  Update the widget(s) of a single TStatusBar panel.
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.OldUpdateStatusBarPanel(StatusBar: TObject; Index: integer;
  StatusPanelWidget: PGtkWidget);
var
  AStatusBar: TStatusBar;
  CurPanel: TStatusPanel;
  FrameWidget: PGtkWidget;
  LabelWidget: PGtkLabel;
  PanelText: String;
  ContextID: LongWord;
  NewShadowType: TGtkShadowType;
  NewJustification: TGtkJustification;
begin
  //DebugLn('TGtkWidgetSet.UpdateStatusBarPanel ',HexStr(Cardinal(StatusBar),8),' Index=',dbgs(Index));
  AStatusBar:=StatusBar as TStatusBar;

  CurPanel:=nil;
  if (not AStatusBar.SimplePanel) and (AStatusBar.Panels.Count>Index) then
    CurPanel:=AStatusBar.Panels[Index];
  //DebugLn('Panel ',Index,' ',GetWidgetClassName(StatusPanelWidget),
  //  ' frame=',GetWidgetClassName(PGTKStatusBar(StatusPanelWidget)^.frame),
  //  ' thelabel=',GetWidgetClassName(PGTKStatusBar(StatusPanelWidget)^.thelabel),
  // '');
  FrameWidget:=PGTKStatusBar(StatusPanelWidget)^.frame;
  LabelWidget:=PGtkLabel({$ifdef gtk2}PGTKStatusBar(StatusPanelWidget)^._label{$else}PGTKStatusBar(StatusPanelWidget)^.thelabel{$endif});

  // Text
  if AStatusBar.SimplePanel then
    PanelText:=AStatusBar.SimpleText
  else if CurPanel<>nil then
    PanelText:=CurPanel.Text
  else
    PanelText:='';
  ContextID:=gtk_statusbar_get_context_id(PGTKStatusBar(StatusPanelWidget),
                                          'state');
  //DebugLn('  PanelText="',PanelText,'"');
  if PanelText<>'' then
    gtk_statusbar_push(PGTKStatusBar(StatusPanelWidget),ContextID,
                       PGChar(PanelText))
  else
    gtk_statusbar_push(PGTKStatusBar(StatusPanelWidget),ContextID,'');


  // Alignment
  if CurPanel<>nil then begin
    //DebugLn('  Alignment="',ord(CurPanel.Alignment),'"');
    case CurPanel.Alignment of
    taLeftJustify: NewJustification:=GTK_JUSTIFY_LEFT;
    taRightJustify: NewJustification:=GTK_JUSTIFY_RIGHT;
    taCenter: NewJustification:=GTK_JUSTIFY_CENTER;
    else
      NewJustification:=GTK_JUSTIFY_LEFT;
    end;
    gtk_label_set_justify(LabelWidget,NewJustification);
  end;

  // Bevel
  if CurPanel<>nil then begin
    case CurPanel.Bevel of
    pbNone: NewShadowType:=GTK_SHADOW_NONE;
    pbLowered: NewShadowType:=GTK_SHADOW_IN;
    pbRaised: NewShadowType:=GTK_SHADOW_OUT;
    else
      NewShadowType:=GTK_SHADOW_IN;
    end;
    gtk_frame_set_shadow_type(PGtkFrame(FrameWidget),NewShadowType);
  end;

  // Width
  if (CurPanel<>nil) then begin
    //DebugLn('  CurPanel.Width="',CurPanel.Width,'"');
    gtk_widget_set_usize(StatusPanelWidget,CurPanel.Width,
      StatusPanelWidget^.allocation.height);
  end;
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
  Result := gtk_hbox_new(false, 0);
  TempWidget := gtk_fixed_new();
  gtk_container_add(GTK_CONTAINER(Result), TempWidget);
  gtk_widget_show(TempWidget);
  if NotOnParentsClientArea then begin
    WinWidgetInfo:=GetWidgetInfo(Result,true);
    Include(WinWidgetInfo^.Flags,wwiNotOnParentsClientArea);
  end;
  SetFixedWidget(Result, TempWidget);
  SetMainWidget(Result, TempWidget);
  gtk_widget_show(Result);
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
  Result := gtk_toolbar_new();
  ClientWidget := gtk_fixed_new();
  gtk_toolbar_insert_widget(PGTKToolbar(Result),ClientWidget,nil,nil,0);
  gtk_widget_show(ClientWidget);
  SetFixedWidget(Result,ClientWidget);
  SetMainWidget(Result,ClientWidget);
  {$IFDEF GTK1}
  gtk_toolbar_set_space_size(PGTKToolbar(Result),0);
  gtk_toolbar_set_space_style(PGTKToolbar(Result),GTK_TOOLBAR_SPACE_EMPTY);
  {$ENDIF GTK1}
  gtk_widget_show(Result);
end;

{------------------------------------------------------------------------------
  Function: TGtkWidgetSet.CreateComponent
  Params:   sender - object for which to create visual representation
  Returns:  nothing

  Tells GTK Engine to create a widget
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreateComponent(Sender : TObject): THandle;
var
  Caption : ansistring;          // the caption of "Sender"
  StrTemp : PChar;               // same as "caption" but as PChar
  TempWidget,
  TempWidget2 : PGTKWidget;      // pointer to gtk-widget (local use when neccessary)
  p          : pointer;          // ptr to the newly created GtkWidget
  CompStyle,                     // componentstyle (type) of GtkWidget which will be created
  TempInt   : Integer;           // local use when neccessary

  Box       : Pointer;           // currently only used for MainMenu
  ParentForm: TCustomForm;
  AccelText : PChar;
  AccelKey  : guint;
  SetupProps : boolean;
  AWindow: PGdkWindow;
begin
  p := nil;
  SetupProps:= false;

  CompStyle := GetCompStyle(Sender);
  Caption   := GetCaption(Sender);

  strTemp := StrAlloc(length(Caption) + 1);
  StrPCopy(strTemp, Caption);

  case CompStyle of
  csAlignment :
    begin
      p := gtk_alignment_new(0.5,0.5,0,0);
      gtk_widget_show(p);
    end;

  csArrow :
    begin
      p := gtk_arrow_new(gtk_arrow_left,gtk_shadow_etched_in);
    end;

  csBitBtn,
  csButton: DebugLn('[WARNING] Obsolete call to TGTKOBject.CreateComponent for ', Sender.ClassName);

  csCalendar :
    begin
      p := gtk_calendar_new();
    end;

  csCheckbox :
    begin
      p := gtk_check_button_new_with_label(strTemp);
    end;

  csClistBox :
    {$IFdef GTK2}
    begin
      p:= gtk_scrolled_window_new(nil, nil);//give something just in case
      GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.hscrollbar, GTK_CAN_FOCUS);
      GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.vscrollbar, GTK_CAN_FOCUS);
      gtk_scrolled_window_set_policy(PGtkScrolledWindow(p),
                                    GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
      gtk_widget_show(p);

      DebugLn('TODO: TGtkWidgetSet.CreateComponent csCListBox');
    end;
    {$Else}
    begin
      p:= gtk_scrolled_window_new(nil, nil);
      GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.hscrollbar, GTK_CAN_FOCUS);
      GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.vscrollbar, GTK_CAN_FOCUS);
      gtk_scrolled_window_set_policy(PGtkScrolledWindow(p),
                                     GTK_POLICY_AUTOMATIC,
                                     GTK_POLICY_AUTOMATIC);
      gtk_widget_show(p);

      with TCListBox(Sender)
      do begin
        TempWidget:= gtk_clist_new(ListColumns);
        //gtk_container_add(PGtkContainer(p), TempWidget);
        for TempInt := 0 to ListColumns - 1 do
          gtk_clist_set_column_width(PGtkCList(TempWidget), TempInt,
                                             (Max(0,Width-10)) div ListColumns);
      end;
      gtk_scrolled_window_add_with_viewport(PGtkScrolledWindow(p), TempWidget);
      gtk_container_set_focus_vadjustment(PGtkContainer(TempWidget),
                    gtk_scrolled_window_get_vadjustment(PGtkScrolledWindow(p)));
      gtk_container_set_focus_hadjustment(PGtkContainer(TempWidget),
                    gtk_scrolled_window_get_hadjustment(PGtkScrolledWindow(p)));
      gtk_widget_show(TempWidget);

      SetMainWidget(p, TempWidget);
      GetWidgetInfo(p, True)^.CoreWidget := TempWidget;
      SetSelectionMode(Sender,p,TCListBox(Sender).MultiSelect,
                       TCListBox(Sender).ExtendedSelect)
    end;
    {$EndIf}

  csColorDialog :
    begin
      P := gtk_color_selection_dialog_new(StrTemp);
      g_signal_connect( gtk_object((GTK_COLOR_SELECTION_DIALOG(P))^.ok_button),
        'clicked', gtk_signal_func(@gtkDialogOKclickedCB), Sender);
      g_signal_connect( gtk_object((GTK_COLOR_SELECTION_DIALOG(P))^.cancel_button),
        'clicked', gtk_signal_func(@gtkDialogCancelclickedCB), Sender);
      InitializeCommonDialog(TCommonDialog(Sender),p);
    end;

  csComboBox :
    p:=CreateComboBox(TComboBox(Sender));

  {$IfDef GTK1}
  csEdit :
    p :=  gtk_entry_new();
  {$EndIF}
  csFileDialog, csOpenFileDialog, csSaveFileDialog, csSelectDirectoryDialog,
  csPreviewFileDialog:
    InitializeFileDialog(TFileDialog(Sender),p,StrTemp);

  csFontDialog :
    InitializeFontDialog(TFontDialog(Sender),p,StrTemp);

  csWinControl:
    p:=CreateAPIWidget(TWinControl(Sender));

  csForm:
    p:=CreateForm(TCustomForm(Sender));
(*
  csFrame :
    begin
      P := gtk_frame_new(' ');
      gtk_frame_set_shadow_type(pGtkFrame(P),GTK_SHADOW_NONE);
    end;
*)
  csGroupBox:
    begin
      P := gtk_frame_new (StrTemp);
      TempWidget := gtk_fixed_new();
      gtk_container_add(GTK_CONTAINER(p), TempWidget);
      gtk_widget_show(TempWidget);
      SetFixedWidget(p, TempWidget);
      SetMainWidget(p, TempWidget);
      gtk_widget_show (P);
    end;

  csHintWindow :
    Begin
       p := gtk_window_new(gtk_window_popup);
       gtk_window_set_policy (GTK_WINDOW (p), 0, 0, 0);

       // Create the form client area
       TempWidget := gtk_fixed_new();
       gtk_container_add(p, TempWidget);
       gtk_widget_show(TempWidget);
       SetFixedWidget(p, TempWidget);
       SetMainWidget(p, TempWidget);

       TCustomForm(Sender).FormStyle := fsStayOnTop;
       TCustomForm(Sender).BorderStyle := bsNone;
       gtk_widget_realize(p);
       AWindow:=GetControlWindow(P);
        {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
       gdk_window_set_decorations(AWindow,
         GetWindowDecorations(TCustomForm(Sender)));
       gdk_window_set_functions(AWindow,
         GetWindowFunction(TCustomForm(Sender)));
       {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
       gtk_widget_show_all(p);
    end;

  csImage :
    Begin
      p := gtk_image_new();
    end;


  csStaticText:
    begin
      P := gtk_label_new(StrTemp);
      SetLabelAlignment(PGtkLabel(p),TCustomStaticText(Sender).Alignment,
                        TCustomStaticText(Sender).Layout);
      SetupProps:= true;
    end;

  csListBox, csCheckListBox:
    {$IFdef GTK2}
    begin
      p:= gtk_scrolled_window_new(nil, nil);//give something just in case
      GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.hscrollbar, GTK_CAN_FOCUS);
      GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.vscrollbar, GTK_CAN_FOCUS);
      gtk_scrolled_window_set_policy(PGtkScrolledWindow(p),
                                    GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
      gtk_widget_show(p);

      DebugLn('TODO: TGtkWidgetSet.CreateComponent csListBox, csCheckListBox');
    end;
    {$Else}
    begin
      p:= gtk_scrolled_window_new(nil, nil);
      GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.hscrollbar, GTK_CAN_FOCUS);
      GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.vscrollbar, GTK_CAN_FOCUS);
      gtk_scrolled_window_set_policy(PGtkScrolledWindow(p),
                                    GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
      gtk_widget_show(p);
      TempWidget:= gtk_list_new;
      gtk_scrolled_window_add_with_viewport(PGtkScrolledWindow(p), TempWidget);
      gtk_container_set_focus_vadjustment(PGtkContainer(TempWidget),
                   gtk_scrolled_window_get_vadjustment(PGtkScrolledWindow(p)));
      gtk_container_set_focus_hadjustment(PGtkContainer(TempWidget),
                   gtk_scrolled_window_get_hadjustment(PGtkScrolledWindow(p)));
      gtk_widget_show(TempWidget);

      SetMainWidget(p, TempWidget);
      GetWidgetInfo(p, True)^.CoreWidget := TempWidget;
      if Sender is TCustomListBox then
        SetSelectionMode(Sender,p,TCustomListBox(Sender).MultiSelect,
                         TCustomListBox(Sender).ExtendedSelect);
    end;
   {$EndIf}

  csListView :
    Begin
      p:=CreateListView(Sender);
    end;


  csMainMenu:
    begin
      p := gtk_menu_bar_new();
      // get the VBox, the form has one child, a VBox
      ParentForm:=TCustomForm(TMenu(Sender).Parent);
      if (ParentForm=nil) or (not (ParentForm is TCustomForm)) then
        RaiseException('MainMenu without form');
      if ParentForm.Menu<>TMenu(Sender) then
        RaiseException('form has already a MainMenu');
      if ParentForm.HandleAllocated then begin
        Box := PGTKBin(ParentForm.Handle)^.Child;
        gtk_box_pack_start(Box, p, False, False, 0);
      end;
      gtk_widget_show(p);
    end;

  {$IfDef GTK1}
  csMemo :
    begin
      P := gtk_scrolled_window_new(nil, nil);
      TempWidget := gtk_text_new(nil, nil);
      gtk_container_add(p, TempWidget);

      GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.hscrollbar, GTK_CAN_FOCUS);
      GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.vscrollbar, GTK_CAN_FOCUS);
      gtk_scrolled_window_set_policy(PGtkScrolledWindow(p),
                                     GTK_POLICY_AUTOMATIC,
                                     GTK_POLICY_AUTOMATIC);
      gtk_text_set_adjustments(PGtkText(TempWidget),
        gtk_scrolled_window_get_hadjustment(PGtkScrolledWindow(p)),
        gtk_scrolled_window_get_vadjustment(PGtkScrolledWindow(p)));

      SetMainWidget(p, TempWidget);
      GetWidgetInfo(p, True)^.CoreWidget := TempWidget;

      gtk_text_set_editable (PGtkText(TempWidget), not TCustomMemo(Sender).ReadOnly);
      if TCustomMemo(Sender).WordWrap then
        gtk_text_set_line_wrap(PGtkText(TempWidget), GdkTrue)
      else
        gtk_text_set_line_wrap(PGtkText(TempWidget), GdkFalse);
      gtk_text_set_word_wrap(PGtkText(TempWidget), GdkTrue);

      gtk_widget_show_all(P);

      SetupProps:= true;
    end;
  {$EndIF}
  csMenuBar :
    begin
       P := gtk_menu_bar_new();
       gtk_container_add(
         GTK_Container(
           GetFixedWidget(Pointer(TWinControl(TMenu(Sender).Owner).Handle))), P);
       gtk_widget_show(p);
    end;

  csMenuItem :
    p:=CreateMenuItem(TMenuItem(Sender));

  csNotebook :
    begin
      P := gtk_notebook_new();
      gtk_notebook_set_scrollable(P, true);
      gtk_notebook_popup_enable(P);
      if TCustomNotebook(Sender).PageCount=0 then
        // a gtk notebook needs a page
        // -> add dummy page
        AddDummyNoteBookPage(PGtkNotebook(p));
    end;

  csPage:    // TCustomPage - Notebook page
    P:=CreateSimpleClientAreaWidget(Sender,true);

  csPairSplitter:
    p:=CreatePairSplitter(Sender);

  csPairSplitterSide:
    P:=CreateSimpleClientAreaWidget(Sender,true);

  csPanel:
    begin
      // create a fixed widget in a horizontal box
      p := gtk_hbox_new(false,0);
      TempWidget := gtk_fixed_new();
      gtk_container_add(GTK_CONTAINER(P), TempWidget);
      gtk_widget_show(TempWidget);
      SetFixedWidget(p, TempWidget);
      SetMainWidget(p, TempWidget);
      gtk_widget_show(P);
    end;

  csPopupMenu :
    with (TPopupMenu(Sender)) do
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
      AccelText := Ampersands2Underscore(StrTemp);
      if TempWidget <> nil then
        P:= gtk_radio_button_new_with_label(PGtkRadioButton(TempWidget)^.group,
                                            AccelText)
      else
        P:= gtk_radio_button_new_with_label(nil, AccelText);
      AccelKey:=gtk_label_parse_uline(
                                   pGtkLabel(gtk_bin_get_child(@PGTKToggleButton(P)^.Button)),
                                   AccelText);
      Accelerate(TComponent(Sender),PGtkWidget(P),AccelKey,0,'clicked');
      StrDispose(AccelText);
    end;

  csScrollBar :
    begin
      if (TScrollBar(sender).kind = sbHorizontal) then
      begin
        P := gtk_hscrollbar_new(PgtkAdjustment(
                 gtk_adjustment_new(1,TScrollBar(sender).min,
                 TScrollBar(sender).max,
                 TScrollBar(sender).SmallChange, TScrollBar(sender).LargeChange,
                 TScrollBar(sender).Pagesize)));
      end
      else
      Begin
        P := gtk_vscrollbar_new(PgtkAdjustment(
                 gtk_adjustment_new(1,TScrollBar(sender).min,
                 TScrollBar(sender).max,
                 TScrollBar(sender).SmallChange, TScrollBar(sender).LargeChange,
                 TScrollBar(sender).Pagesize)));
      end;
    end;

  csScrolledWindow :
    begin
       P := gtk_scrolled_window_new(nil,nil);
    end;

  csSpeedButton:
    Begin
      p := gtk_button_new_with_label(StrTemp);
    end;

  csSpinEdit :
    begin
      p := gtk_spin_button_new(PgtkAdjustment(
                                        gtk_adjustment_new(1,1,100,1,1,1)),1,0);
    end;

  csStatusBar :
    begin
      p:=CreateStatusBar(Sender);
    end;

  csToggleBox :
    begin
      P := gtk_toggle_button_new_with_label(StrTemp);
    end;

  csToolbar:
    P:=CreateToolBar(Sender);

  csToolButton:
    begin
      p := gtk_fixed_new();
    end;

  csTrackBar:
    with (TCustomTrackBar (Sender)) do
    begin
       TempWidget := PGtkWidget( gtk_adjustment_new (Position, Min, Max, linesize, pagesize, 0));
       if (Orientation = trHorizontal) then
         P := gtk_hscale_new (PGTKADJUSTMENT (TempWidget))
         else
         P := gtk_vscale_new (PGTKADJUSTMENT (TempWidget));
       gtk_scale_set_digits (PGTKSCALE (P), 0);
    end;

  csScrollBox :
    begin
      Assert(Sender is TScrollBox);

      P := gtk_frame_new(nil);
      gtk_frame_set_shadow_type(pGtkFrame(P),GTK_SHADOW_IN);

      TempWidget := gtk_scrolled_window_new(nil,nil);
      gtk_container_add(PGTKContainer(P), TempWidget);
      gtk_widget_show(TempWidget);

      gtk_object_set_data(P,odnScrollArea, TempWidget);

      TempWidget2 := gtk_layout_new(nil, nil);
      gtk_container_add(PGTKContainer(TempWidget), TempWidget2);
      gtk_widget_show(TempWidget2);
      SetFixedWidget(p, TempWidget2);
      SetMainWidget(p, TempWidget2);

      GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(TempWidget)^.hscrollbar, GTK_CAN_FOCUS);
      GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(TempWidget)^.vscrollbar, GTK_CAN_FOCUS);
      gtk_scrolled_window_set_policy(PGtkScrolledWindow(TempWidget),
                                     GTK_POLICY_NEVER,
                                     GTK_POLICY_NEVER);
    end;

  end; //end case
  StrDispose(StrTemp);
  FinishComponentCreate(Sender, P, SetupProps);
  Result := THandle(P);
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
    RaiseException('TGtkWidgetSet.DestroyEmptySubmenu');
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
procedure TGtkWidgetSet.ShowHide(Sender : TObject);

  procedure RaiseWrongClass;
  begin
    RaiseException('TGtkWidgetSet.ShowHide Sender.ClassName='+Sender.ClassName);
  end;

var FormIconGdiObject: PGDIObject;
  SenderWidget, ParentFixed, ParentWidget: PGTKWidget;
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

    if gtk_widget_visible(SenderWidget) then
      exit;

    // before making the widget visible, set the position and size
    if FWidgetsWithResizeRequest.Contains(SenderWidget) then begin
      if (ACustomForm<>nil) and (ACustomForm.Parent=nil) then begin
        // top level control (a form without parent)
        {$IFDEF VerboseFormPositioning}
        DebugLn('VFP [TGtkWidgetSet.ShowHide] A set bounds ',
          LCLControl.Name,':',LCLControl.ClassName,
          ' Window=',GetControlWindow(SenderWidget)<>nil,
          ' ',LCLControl.Left,',',LCLControl.Top,
          ',',LCLControl.Width,',',LCLControl.Height);
        {$ENDIF}
        SetWindowSizeAndPosition(PgtkWindow(SenderWidget),LCLControl);
      end else if (LCLControl.Parent<>nil) then begin
        // resize widget
        RealizeWidgetSize(SenderWidget,LCLControl.Width,LCLControl.Height);
        // move widget on the fixed widget of parent control
        ParentWidget:=pgtkWidget(LCLControl.Parent.Handle);
        ParentFixed := GetFixedWidget(ParentWidget);
        if GtkWidgetIsA(ParentFixed,GTK_FIXED_GET_TYPE)
        or GtkWidgetIsA(ParentFixed,GTK_LAYOUT_GET_TYPE) then begin
          FixedMoveControl(ParentFixed, SenderWidget,
                         LCLControl.Left, LCLControl.Top);
        end else if not (LCLControl.Parent is TCustomNoteBook) then begin
          DebugLn('WARNING: TGtkWidgetSet.ShowHide - no Fixed Widget found');
          DebugLn('  Control=',LCLControl.Name,':',LCLControl.ClassName);
        end;
      end;
      UnsetResizeRequest(SenderWidget);
    end;

    if (ACustomForm<>nil) and (ACustomForm.Parent=nil) then begin
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

    gtk_widget_show(SenderWidget);

    if (ACustomForm<>nil) and (ACustomForm.Parent=nil) then begin
      AWindow:=GetControlWindow(SenderWidget);
      if (AWindow<>nil) and (ACustomForm.Icon<>nil) then begin
        FormIconGdiObject:=PGDIObject(ACustomForm.GetIconHandle);
        if (FormIconGdiObject<>nil) then begin
          gdk_window_set_icon(AWindow, nil,
                  FormIconGdiObject^.GDIBitmapObject,
                  FormIconGdiObject^.GDIBitmapMaskObject);
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
      UntransientWindow(PGtkWindow(SenderWIdget));
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
{$IfNDef NoGdkPixbufLib}
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
  pixbuf:=gdk_pixbuf_new_from_xpm_data(ImgData);
  {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
  FreeMem(ImgData);
end;
{$EndIF}

{-------------------------------------------------------------------------------
  procedure AddDummyNoteBookPage(NoteBookWidget: PGtkNoteBook);

  Adds the dummy page.
  A gtk notebook must have at least one page, but TCustomNoteBook also allows
  no pages at all. Therefore at least a dummy page is added. This dummy page is
  removed as soon as other pages are added.
-------------------------------------------------------------------------------}
procedure TGtkWidgetSet.AddDummyNoteBookPage(NoteBookWidget: PGtkNoteBook);
var
  DummyWidget, AWidget, ALabel, MenuLabel: PGtkWidget;
begin
  if NoteBookWidget=nil then exit;
  DummyWidget:=GetGtkNoteBookDummyPage(NoteBookWidget);
  if (DummyWidget=nil) then begin
    // the notebook has no pages
    // -> add a dummy page
    DummyWidget := gtk_hbox_new(false, 0);
    AWidget := gtk_fixed_new;
    gtk_widget_show(AWidget);
    //gtk_box_pack_start_defaults(GTK_BOX(DummyWidget),AWidget);
    gtk_container_add(GTK_CONTAINER(DummyWidget), AWidget);
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
  aDC     : TDeviceContext;
  DCOrigin: TPoint;
  GDKColor: TGDKColor;
begin
  aDC := TDeviceContext(CanvasHandle);
  if (aDC = nil) or (aDC.Drawable = nil) then exit;

  DCOrigin:=GetDCOffset(aDC);
  inc(X,DCOrigin.X);
  inc(Y,DCOrigin.Y);

  aDC.SelectedColors := dcscCustom;
  GDKColor:=AllocGDKColor(AColor);
  gdk_gc_set_foreground(aDC.GC, @GDKColor);
  {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
  gdk_draw_point(aDC.Drawable, aDC.GC, X, Y);
  {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
end;

procedure TGtkWidgetSet.DCRedraw(CanvasHandle: HDC);
var
  fWindow :pGdkWindow;
  widget : PgtkWIdget;
  PixMap : pgdkPixMap;
  //gc : PGDKGc;
  Child: PGtkWidget;
begin
  Assert(False, 'Trace:In AutoRedraw in GTKObject');
  
  Child := PgtkWidget(CanvasHandle);
  Widget := GetFixedWidget(Child);
  pixmap := gtk_Object_get_data(pgtkobject(Child),'Pixmap');
  if PixMap = nil then Exit;
  fWindow := GetControlWindow(widget);
  //gc := gdk_gc_new(PgdkWindow(fWindow));

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
  aDC    : TDeviceContext;
  Image  : pGDKImage;
  GDKColor: TGDKColor;
  Colormap : PGDKColormap;
  DCOrigin: TPoint;
  MaxX, MaxY: integer;
  Pixel: LongWord;
begin
  Result := clNone;
  aDC := TDeviceContext(CanvasHandle);
  if (aDC = nil) or (aDC.Drawable = nil) then exit;

  DCOrigin:=GetDCOffset(TDeviceContext(aDC));
  inc(X,DCOrigin.X);
  inc(Y,DCOrigin.Y);

  gdk_drawable_get_size(aDC.Drawable, @MaxX, @MaxY);
  if (X<0) or (Y<0) or (X>=MaxX) or (Y>=MaxY) then exit;

  Image := gdk_drawable_get_image(aDC.Drawable,X,Y,1,1);
  if Image = nil then exit;

  colormap := gdk_image_get_colormap(image);

  if colormap = nil then
    colormap := gdk_drawable_get_colormap(aDC.Drawable);

  if colormap = nil then
    colormap := gdk_colormap_get_system;

  Pixel:=gdk_image_get_pixel(Image,0,0);
  FillChar(GDKColor,SizeOf(GDKColor),0);
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

  Checks if the given GDIObject is valid
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.IsValidGDIObject(const GDIObject: HGDIOBJ): Boolean;
begin
  Result := (GDIObject<>0) and (FGDIObjects.Contains(Pointer(GDIObject)));
  if Result then
    with PGdiObject(GDIObject)^ do
      case GDIType of
        gdiBitmap : begin
           case GDIBitmapType of
             gbPixmap: Result := GDIPixmapObject <> nil;
             gbBitmap: Result := GDIBitmapObject <> nil;
             {obsolete: gbImage:  Result := GDI_RGBImageObject <> nil;}
           else
             Result := False;
           end;
        end;
        gdiBrush  : Result := True; //Result := GDIBrushPixmap <> nil; //GDIBrushPixmap may be nil
        gdiFont   : Result := GDIFontObject <> nil;
        gdiPen    : Result := True;
        gdiRegion : Result := True;
      else
        Result := False;
      end;
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
  Procedure: TGtkWidgetSet.SelectGDKBrushProps
  Params:  DC: a (LCL)devicecontext
  Returns: Nothing

  Sets the forecolor and fill according to the brush
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.SelectGDKBrushProps(DC: HDC);
begin
  if (TDeviceContext(DC).SelectedColors=dcscBrush) or
     TDeviceContext(DC).CurrentBrush^.IsNullBrush
  then
    exit;

  with TDeviceContext(DC), CurrentBrush^ do
  begin
    //DebugLn('TGtkWidgetSet.SelectGDKBrushProps Setting BKColor ...');
    EnsureGCColor(DC, dccCurrentBackColor, True, True);//BKColor
    //DebugLn('TGtkWidgetSet.SelectGDKBrushProps Setting Brush Color ...');
    EnsureGCColor(DC, dccGDIBrushColor, GDIBrushFill = GDK_Solid, False);//Brush Color

    If GDIBrushFill <> GDK_Solid then
      If GDIBrushPixmap <> nil then begin
        gdk_gc_set_fill(GC, GDIBrushFill);
        gdk_gc_set_Stipple(GC,GDIBrushPixmap);
      end
  end;
  TDeviceContext(DC).SelectedColors:=dcscBrush;
end;

{------------------------------------------------------------------------------
  Procedure: TGtkWidgetSet.SelectGDKTextProps
  Params:  DC: a (LCL)devicecontext
  Returns: Nothing

  Sets the forecolor and fill according to the Textcolor
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.SelectGDKTextProps(DC: HDC);
begin
  if TDeviceContext(DC).SelectedColors=dcscFont then exit;

  with TDeviceContext(DC) do
  begin
    EnsureGCColor(DC, dccCurrentBackColor, True, True);//BKColor
    EnsureGCColor(DC, dccCurrentTextColor, False, False);//Font Color
  end;
  TDeviceContext(DC).SelectedColors:=dcscFont;
end;

{------------------------------------------------------------------------------
  Procedure: TGtkWidgetSet.TGtkWidgetSet.SelectGDKPenProps
  Params:  DC: a (LCL)devicecontext
  Returns: Nothing

  Sets the forecolor and fill according to the pen
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.SelectGDKPenProps(DC: HDC);

  procedure SetDashes(const Dashes: array of gint8);
  begin
    {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
    laz_gdk_gc_set_dashes(TDeviceContext(DC).GC,0,Pgint8(@Dashes[Low(Dashes)]),
      High(Dashes)-Low(Dashes)+1);
    {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
  end;

begin
  if TDeviceContext(DC).SelectedColors<>dcscPen then begin
    with TDeviceContext(DC), CurrentPen^ do begin
      EnsureGCColor(DC, dccCurrentBackColor, True, True);//BKColor
      EnsureGCColor(DC, dccGDIPenColor, False, False);//Pen Color
    end;
    TDeviceContext(DC).SelectedColors:=dcscPen;
  end;

  if (not (dcfPenSelected in TDeviceContext(DC).DCFlags)) then begin
    Exclude(TDeviceContext(DC).DCFlags,dcfPenInvalid);
    if TDeviceContext(DC).GC<>nil then begin
      with TDeviceContext(DC), CurrentPen^ do
      begin
        IsNullPen := GDIPenStyle = PS_NULL;
        if (GDIPenStyle = PS_SOLID) or (GDIPenStyle = PS_INSIDEFRAME)
        then begin
          {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
          gdk_gc_set_line_attributes(GC, GDIPenWidth, GDK_LINE_SOLID, GDK_CAP_NOT_LAST, GDK_JOIN_MITER);
          {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
        end
        else begin
          {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
          gdk_gc_set_line_attributes(GC,GDIPenWidth,GDK_LINE_ON_OFF_DASH,GDK_CAP_NOT_LAST,GDK_JOIN_MITER);
          case GDIPenStyle of
            {$IfDef GTK2}
            PS_DASH:       SetDashes([#4,#4]);
            PS_DOT:        SetDashes([#2,#2]);
            PS_DASHDOT:    SetDashes([#4,#2,#2,#2]);
            PS_DASHDOTDOT: SetDashes([#4,#2,#2,#2,#2,#2]);
            {$Else}
            PS_DASH:       SetDashes([4,4]);
            PS_DOT:        SetDashes([2,2]);
            PS_DASHDOT:    SetDashes([4,2,2,2]);
            PS_DASHDOTDOT: SetDashes([4,2,2,2,2,2]);
            {$EndIf}
            //This is DEADLY!!!
            //PS_NULL:       gdk_gc_set_dashes(GC, 0, [0,4], 2);
          end;
          {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
        end;
      end;
      Include(TDeviceContext(DC).DCFlags,dcfPenSelected);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: NewDC
  Params:  none
  Returns: a gtkwinapi DeviceContext

  Creates an initial DC
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.NewDC: TDeviceContext;
begin
  Assert(False, Format('Trace:> [TGtkWidgetSet.NewDC]', []));
  Result:=GtkDef.NewDeviceContext;
  with Result do
  begin
    gdk_color_black(gdk_colormap_get_system, @CurrentTextColor.Color);
    BuildColorRefFromGDKColor(CurrentTextColor);
    gdk_color_white(gdk_colormap_get_system, @CurrentBackColor.Color);
    BuildColorRefFromGDKColor(CurrentBackColor);
  end;
  FDeviceContexts.Add(Result);
//DebugLn('[TGtkWidgetSet.NewDC] ',HexStr(Cardinal(Result),8),'  ',FDeviceContexts.Count);
//  Assert(False, Format('Trace:< [TGtkWidgetSet.NewDC] FDeviceContexts[%d] --> 0x%p', [n, Result]));
end;

{------------------------------------------------------------------------------
  procedure TGtkWidgetSet.DisposeDC(DC: PDeviceContext);

  Disposes a DC
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.DisposeDC(aDC: TDeviceContext);
begin
  if FDeviceContexts.Contains(aDC) then begin
    FDeviceContexts.Remove(aDC);
    GtkDef.DisposeDeviceContext(aDC);
  end;
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.CreateDCForWidget(TheWidget: PGtkWidget;
    TheWindow: PGdkWindow; WithChildWindows: boolean): HDC;

  Creates an initial DC
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CreateDCForWidget(TheWidget: PGtkWidget;
  TheWindow: PGdkWindow; WithChildWindows: boolean): HDC;

  procedure RaiseWidgetWithoutClientArea;
  begin
    RaiseException('TGtkWidgetSet.CreateWindowDC widget '
      +HexStr(Cardinal(TheWidget),8)+' has no client area');
  end;

var
  aDC: TDeviceContext;
  ClientWidget: PGtkWidget;
  GdiObject: PGdiObject;
  GCValues: TGdkGCValues;
begin
  aDC := nil;

  aDC := NewDC;
  aDC.Wnd := HWND(TheWidget);

  GdiObject := nil;

  if TheWidget = nil
  then begin
    FillChar(GCValues, SizeOf(GCValues), #0);
  end
  else begin
    // create a new devicecontext for this window
    if TheWindow=nil then begin
      ClientWidget := GetFixedWidget(TheWidget);
      if ClientWidget = nil then RaiseWidgetWithoutClientArea;
      TheWindow:=GetControlWindow(ClientWidget);
      if TheWindow=nil then begin
        //force creation
        gtk_widget_realize(ClientWidget);
        TheWindow := GetControlWindow(ClientWidget);
        if TheWindow=nil then
          RaiseException('TGtkWidgetSet.CreateDCForWidget: Unable to realize GdkWindow');
      end;
    end else
      ClientWidget:=TheWidget;
    aDC.SpecialOrigin:=GtkWidgetIsA(ClientWidget,GTK_LAYOUT_GET_TYPE);
    aDC.Drawable := TheWindow;
    // create GC
    if WithChildWindows then begin
      //DebugLn('TGtkWidgetSet.CreateDCForWidget A WithChildWindows');
      FillChar(GCValues, SizeOf(GCValues), #0);
      GCValues.subwindow_mode := GDK_INCLUDE_INFERIORS;
      aDC.GC := gdk_gc_new_with_values(aDC.Drawable,
                                 @GCValues,GDK_GC_FUNCTION or GDK_GC_SUBWINDOW);
    end else begin
      aDC.GC := gdk_gc_new(aDC.Drawable);
    end;
    gdk_gc_set_function(aDC.GC, GDK_COPY);

    gdk_gc_get_values(aDC.GC, @GCValues);
    {$Ifdef GTK2}
    // ToDo: create font on demand
    if (gtk_widget_get_style(ClientWidget)<>nil) and
      (gtk_widget_get_style(ClientWidget)^.font_desc <> nil)
    then begin
       GdiObject:=NewGDIObject(gdiFont);
       GdiObject^.GDIFontObject :=  pango_font_description_copy(gtk_widget_get_style(ClientWidget)^.font_desc);
       GdiObject^.StrikeOut := False;
       GdiObject^.Underline := False;
    end else GdiObject := CreateDefaultFont;
    {$EndIf}
  end;

  if aDC <> nil
  then begin
    {$Ifdef GTK1}
    // ToDo: create font on demand
    if GCValues.Font <> nil
    then begin
      GdiObject:=NewGDIObject(gdiFont);
      GdiObject^.GDIFontObject := GCValues.Font;
      FontCache.Reference(GdiObject^.GDIFontObject);
    end
    else GdiObject := CreateDefaultFont;
    {$EndIf}

    If GdiObject = nil then
      GdiObject := CreateDefaultFont;

    aDC.CurrentFont := GdiObject;
    aDC.CurrentBrush := CreateDefaultBrush;
    aDC.CurrentPen := CreateDefaultPen;
  end;

  Result := HDC(aDC);
  Assert(False, Format('trace:< [TGtkWidgetSet.GetDC] Got 0x%x', [Result]));
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
  DevContext: TDeviceContext;
  CaretWasVisible: Boolean;
  MainWidget: PGtkWidget;
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
    WidgetInfo^.DoubleBuffer:=DoubleBuffer;
    BufferCreated:=true;
  end;

  // create DC for double buffer
  Result:=CreateDCForWidget(Widget,PGDKWindow(DoubleBuffer),false);
  DevContext:=TDeviceContext(Result);
  DevContext.OriginalDrawable:=Widget^.Window;
  Include(DevContext.DCFlags,dcfDoubleBuffer);

  if BufferCreated then begin
    // copy old context to buffer
    gdk_gc_set_clip_region(DevContext.GC, nil);
    gdk_gc_set_clip_rectangle(DevContext.GC, nil);

    // hide caret
    HideCaretOfWidgetGroup(Widget,MainWidget,CaretWasVisible);
    // copy
    gdk_window_copy_area(DoubleBuffer, DevContext.GC,0,0,
      Widget^.Window,0,0,Width,Height);

    {LCLObject:=GetParentLCLObject(Widget);
    DebugLn('TGtkWidgetSet.GetDoubleBufferedDC ',HexStr(Cardinal(Widget),8),'=',GetWidgetClassName(Widget),' ',HexStr(Cardinal(LCLObject),8));
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
  DebugLn('TGtkWidgetSet.GetDoubleBufferedDC DC=',HexStr(Cardinal(Result),8));
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
  Result:=GtkDef.NewPGDIObject;
  Result^.GDIType := GDIType;
  inc(Result^.RefCount);
  FGDIObjects.Add(Result);
//DebugLn('[TGtkWidgetSet.NewGDIObject] ',HexStr(Cardinal(Result),8),'  ',FGDIObjects.Count);
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
    GtkDef.DisposePGDIObject(GDIObject);
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
  debugln('TGtkWidgetSet.CreateDefaultBrush Created: ',HexStr(Cardinal(Result),8));
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
begin
  Result := NewGDIObject(gdiFont);
{$Ifdef GTK2}
  Result^.GDIFontObject:= GetDefaultFontDesc(true);
{$Else}
  Result^.GDIFontObject:= GetDefaultFont(true);
{$EndIf}
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

{------------------------------------------------------------------------------
  procedure TGtkWidgetSet.UpdateDCTextMetric(DC: TDeviceContext);

  Sets the gtk resource file and parses it.
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.UpdateDCTextMetric(DC: TDeviceContext);
{$Ifdef GTK2}
begin
end;
{$Else}
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
  UseFont : PGDKFont;
  UnRef : Boolean;
  Width: LongInt;

  {procedure UseWidthHeuristic;
  var
    i: Integer;
    l: Integer;
    lBearing: LongInt;
    rBearing: LongInt;
    tmAscent: Longint;
    tmDescent: Longint;
    CurWidth: integer;
    PC: PGdkWChar;
  begin
    l:=length(TestString[false]);
    for i:=1 to l do begin
      PC:=PGdkWChar(@TestString[true][i*2-1]);
      gdk_text_extents_wc(UseFont, PC,
        2, @lBearing, @rBearing, @CurWidth,
        @tmAscent, @tmDescent);
      debugln('UseWidthHeuristic i=',dbgs(i),' lBearing=',dbgs(lBearing),
        ' rBearing=',dbgs(rBearing),' CurWidth=',dbgs(CurWidth),' ',HexStr(ord(PC^),8));
    end;
  end;}

var
  AvgTxtLen: Integer;
  CachedFont: TGdkFontCacheItem;
begin
  with TDeviceContext(DC) do begin
    if dcfTextMetricsValid in DCFlags then begin
      // cache valid
    end else begin
      UnRef := False;
      if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
      then begin
        UseFont := GetDefaultFont(false);
      end
      else begin
        UseFont := CurrentFont^.GDIFontObject;
      end;
      If UseFont = nil then
        DebugLn('WARNING: [TGtkWidgetSet.GetTextMetrics] Missing font')
      else begin
        FillChar(DCTextMetric, SizeOf(DCTextMetric), 0);
        CachedFont:=FontCache.FindGDKFont(UseFont);
        if (CachedFont<>nil) and (CachedFont.MetricsValid) then begin
          DCTextMetric.lBearing:=CachedFont.lBearing;
          DCTextMetric.rBearing:=CachedFont.rBearing;
          DCTextMetric.IsDoubleByteChar:=CachedFont.IsDoubleByteChar;
          DCTextMetric.TextMetric:=CachedFont.TextMetric;
        end
        else with DCTextMetric do begin
          IsDoubleByteChar:=FontIsDoubleByteCharsFont(UseFont);
          AvgTxtLen:=length(TestString[false]);
          if IsDoubleByteChar then begin
            gdk_text_extents_wc(UseFont, PGdkWChar(TestString[IsDoubleByteChar]),
              AvgTxtLen*2, @lBearing, @rBearing, @Width,
              @TextMetric.tmAscent, @TextMetric.tmDescent);
            //debugln('TGtkWidgetSet.UpdateDCTextMetric A IsDoubleByteChar=',dbgs(IsDoubleByteChar),' Width=',dbgs(Width),' AvgTxtLen=',dbgs(AvgTxtLen));
            TextMetric.tmHeight := gdk_text_height(UseFont,
                                     PChar(TestString[IsDoubleByteChar]),
                                     AvgTxtLen*2)
                           {$IfNDef Win32} + TextMetric.tmdescent div 2{$EndIf};
          end else begin
            gdk_text_extents(UseFont, PChar(TestString[IsDoubleByteChar]),
              AvgTxtLen, @lBearing, @rBearing, @Width,
              @TextMetric.tmAscent, @TextMetric.tmDescent);
            TextMetric.tmHeight := gdk_text_height(UseFont,
                                     PChar(TestString[IsDoubleByteChar]),
                                     AvgTxtLen)
                           {$IfNDef Win32} + TextMetric.tmdescent div 2{$EndIf};
          end;
          //if Width<AvgTxtLen then UseWidthHeuristic;
          //TextMetric.tmAscent := TextMetric.tmHeight - TextMetric.tmDescent;
          TextMetric.tmAveCharWidth := Width div AvgTxtLen;
          if TextMetric.tmAveCharWidth<1 then TextMetric.tmAveCharWidth:=1;
          TextMetric.tmMaxCharWidth :=
            Max(gdk_char_width(UseFont, 'W'),
                gdk_char_width(UseFont, 'M')); // temp hack
          if TextMetric.tmMaxCharWidth<TextMetric.tmAveCharWidth then
            TextMetric.tmMaxCharWidth:=TextMetric.tmAveCharWidth;
          {debugln('TGtkWidgetSet.UpdateDCTextMetric A IsDoubleByteChar=',dbgs(IsDoubleByteChar),
            ' lbearing=',dbgs(lBearing),' rbearing=',dbgs(rBearing),
            ' width='+dbgs(width),' tmAscent='+dbgs(TextMetric.tmAscent),
            ' tmDescent='+dbgs(TextMetric.tmdescent),
            ' tmHeight='+dbgs(TextMetric.tmHeight),
            ' AvgTxtLen='+dbgs(AvgTxtLen),
            ' tmMaxCharWidth='+dbgs(TextMetric.tmMaxCharWidth),
            ' tmAveCharWidth='+dbgs(TextMetric.tmAveCharWidth));}
          if (CachedFont<>nil) then begin
            CachedFont.lBearing:=lBearing;
            CachedFont.rBearing:=rBearing;
            CachedFont.IsDoubleByteChar:=IsDoubleByteChar;
            CachedFont.TextMetric:=TextMetric;
            CachedFont.MetricsValid:=true;
          end;
        end;
        If UnRef then
          FontCache.Unreference(UseFont);
      end;
      Include(DCFlags,dcfTextMetricsValid);
    end;
  end;
end;
{$EndIf}
{------------------------------------------------------------------------------
  function TGtkWidgetSet.GetDefaultFont(IncreaseReferenceCount: boolean): PGDKFont;
 ------------------------------------------------------------------------------}
{$Ifdef GTK2}
function TGtkWidgetSet.GetDefaultFontDesc(IncreaseReferenceCount: boolean): PPangoFontDescription;
begin
  if FDefaultFontDesc = nil then begin
    FDefaultFontDesc:=LoadDefaultFontDesc;
    if FDefaultFontDesc = nil then
      raise EOutOfResources.Create(rsUnableToLoadDefaultFont);
  end;
  Result:=FDefaultFontDesc;
  if IncreaseReferenceCount then
    result := pango_font_description_copy(Result);
end;
{$Else}
function TGtkWidgetSet.GetDefaultFont(IncreaseReferenceCount: boolean): PGDKFont;
begin
  if FDefaultFont = nil then begin
    FDefaultFont:=LoadDefaultFont;
    //  gdk_font_load('-adobe-helvetica-medium-r-normal--*-120-*-*-*-*-iso8859-1');
    if FDefaultFont = nil then begin
      FDefaultFont:= gdk_font_load ('fixed');
      if FDefaultFont = nil then
        raise EOutOfResources.Create(rsUnableToLoadDefaultFont);
    end;
    gdk_font_ref(FDefaultFont); // mark as used
  end;
  Result:=FDefaultFont;
  if IncreaseReferenceCount then
    gdk_font_ref(Result);
end;
{$EndIF}

function TGtkWidgetSet.CreateRegionCopy(SrcRGN: hRGN): hRGN;
var
  GDIObject: PGDIObject;
begin
  GDIObject := NewGDIObject(gdiRegion);
  GDIObject^.GDIRegionObject:=gdk_region_copy(PGdiObject(SrcRGN)^.GDIRegionObject);
  Result := hRgn(GDIObject);
end;

function TGtkWidgetSet.DCClipRegionValid(DC: HDC): boolean;
var
  ClipRegion: hRGN;
begin
  Result:=false;
  if not IsValidDC(DC) then exit;
  ClipRegion:=TDeviceContext(DC).ClipRegion;
  if (ClipRegion<>0) and (not IsValidGDIObject(ClipRegion)) then exit;
  Result:=true;
end;

function TGtkWidgetSet.CreateEmptyRegion: hRGN;
var
  GObject: PGdiObject;
begin
  GObject := NewGDIObject(gdiRegion);
  GObject^.GDIRegionObject := gdk_region_new;
  Result := HRGN(GObject);
  //DebugLn('TGtkWidgetSet.CreateEmptyRgn A RGN=',HexStr(Cardinal(Result),8));
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
  TGtkWidgetSet SetResizeRequest
  Params: Widget: PGtkWidget

  Marks the widget to send a ResizeRequest to the gtk.
  When the LCL resizes a control the new bounds will not be set directly, but
  cached. This is needed, because it is common behaviour to set the bounds step
  by step. For example: Left:=10; Top:=10; Width:=100; Height:=50; results in
  SetBounds(10,0,0,0);
  SetBounds(10,10,0,0);
  SetBounds(10,10,100,0);
  SetBounds(10,10,100,50);
  Because the gtk puts all size requests into a queue, it will process the
  requests not immediately, but _after_ all requests. This results in changing
  the widget size four times and everytime the LCL gets a message. If the
  control has childs, this will result resizing the childs four times.
  Therefore LCL size requests for a widget are cached and only the final one is
  sent.
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.SetResizeRequest(Widget: PGtkWidget);
{$IFDEF VerboseSizeMsg}
var
  LCLControl: TWinControl;
{$ENDIF}
begin
  {$IFDEF VerboseSizeMsg}
  LCLControl:=TWinControl(GetLCLObject(Widget));
  DbgOut('PPP TGtkWidgetSet.SetResizeRequest Widget=',HexStr(Cardinal(Widget),8));
  if (LCLControl<>nil) then begin
    if LCLControl is TWinControl then
      DebugLn(' ',LCLControl.Name,':',LCLControl.ClassName)
    else
      DebugLn(' ERROR: ',LCLControl.ClassName);
  end else begin
    DebugLn(' ERROR: LCLControl=nil');
  end;
  {$ENDIF}
  if not FWidgetsWithResizeRequest.Contains(Widget) then
    FWidgetsWithResizeRequest.Add(Widget);
end;

{------------------------------------------------------------------------------
  TGtkWidgetSet UnsetResizeRequest
  Params: Widget: PGtkWidget

  Unset the mark for the Widget to send a ResizeRequest to the gtk.
  LCL size requests for a widget are cached and only the last one is sent. Some
  widgets like forms send a resize request immediately. To avoid sending resize
  requests multiple times they can unset the mark with this procedure.
 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.UnsetResizeRequest(Widget: PGtkWidget);
begin
  FWidgetsWithResizeRequest.Remove(Widget);
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
      DebugLn('  WriteTargetLists WWW ',ClipboardTypeName[c],' ',TargetList<>nil);
      if TargetList<>nil then begin
        TmpList:=TargetList^.List;
        while TmpList<>nil do begin
          Pair:=PGtkTargetPair(TmpList^.Data);
          DebugLn('    WriteTargetLists BBB ',Pair^.Target);
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
  DebugLn('[TGtkWidgetSet.SetClipboardWidget] ',ClipboardWidget<>nil,' -> ',TargetWidget<>nil);
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
        gtk_selection_add_targets(ClipboardWidget,ClipboardTypeAtoms[c],
                  ClipboardTargetEntries[c],ClipboardTargetEntryCnt[c]);
      end;
    end;
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
procedure TGtkWidgetSet.WordWrap(DC: HDC; AText: PChar; MaxWidthInPixel: integer;
  var Lines: PPChar; var LineCount: integer);
var
  {$IfDef GTK2}
  UseFontDesc : PPangoFontDescription;
  {$Else}
  UseFont : PGDKFont;
  {$EndIf}
  UnRef : Boolean;

  function GetLineWidthInPixel(LineStart, LineLen: integer): integer;
  var
    lbearing, rbearing, width, ascent, descent: LongInt;
  begin
    {$IfDef GTK2}
    GetTextExtentIgnoringAmpersands(UseFontDesc, @AText[LineStart], LineLen,
                     @lbearing, @rBearing, @width, @ascent, @descent);
    {$Else}
    GetTextExtentIgnoringAmpersands(UseFont, @AText[LineStart], LineLen,
                     @lbearing, @rBearing, @width, @ascent, @descent);
    {$EndIf}
    Result:=Width;
  end;

  function FindLineEnd(LineStart: integer): integer;
  var
    LineWidth, WordWidth, WordEnd, CharWidth: integer;
  begin
    // first search line break or text break
    Result:=LineStart;
    while not (AText[Result] in [#0,#10,#13]) do inc(Result);
    if Result<=LineStart+1 then exit;

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
          CharWidth:=GetLineWidthInPixel(Result,1);
          inc(LineWidth,CharWidth);
          if LineWidth>MaxWidthInPixel then break;
          inc(Result);
        until false;
        // at least one char
        if Result=LineStart then inc(Result);
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
    with TDeviceContext(DC) do begin
      if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
      then begin
        {$IfDef GTK2}
        UseFontDesc := GetDefaultFontDesc(true);
        {$Else}
        UseFont := GetDefaultFont(true);
        {$EndIf}
        UnRef := True;
      end
      else begin
        {$IfDef GTK2}
        UseFontDesc := CurrentFont^.GDIFontObject;
        {$Else}
        UseFont := CurrentFont^.GDIFontObject;
        {$EndIf}
        UnRef := False;
      end;
    end;
  end;

  procedure CleanUpFont;
  begin
    If UnRef then
      {$IfDef GTK2}
      pango_font_description_free(UseFontDesc);
      {$Else}
      FontCache.Unreference(UseFont);
      {$EndIf}
  end;

var
  LinesList: TList;
  LineStart, LineEnd, LineLen: integer;
  ArraySize, TotalSize: integer;
  i: integer;
  CurLineEntry: PPChar;
  CurLineStart: PChar;
begin
  if IsEmptyText then exit;
  InitFont;
  LinesList:=TList.Create;
  LineStart:=0;

  // find all line starts and line ends
  repeat
    LinesList.Add(Pointer(LineStart));
    // find line end
    LineEnd:=FindLineEnd(LineStart);
    LinesList.Add(Pointer(LineEnd));
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
    LineLen:=Cardinal(LinesList[i+1])-Cardinal(LinesList[i])+1;
    inc(TotalSize,LineLen);
    inc(i,2);
  end;
  GetMem(Lines,TotalSize);

  // create Lines
  CurLineEntry:=Lines;
  CurLineStart:=PChar(CurLineEntry)+ArraySize;
  i:=0;
  while i<LinesList.Count do begin
    // set the pointer to the start of the current line
    CurLineEntry[i shr 1]:=CurLineStart;
    // copy the line
    LineStart:=Integer(LinesList[i]);
    LineEnd:=Integer(LinesList[i+1]);
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
  if Integer(Lines)+TotalSize<>Integer(CurLineStart) then
    RaiseException('TGtkWidgetSet.WordWrap Consistency Error:'
      +' Lines+TotalSize<>CurLineStart');
  CurLineEntry[i shr 1]:=nil;

  LinesList.Free;
  CleanUpFont;
end;

function TGtkWidgetSet.ForceLineBreaks(DC : hDC; Src: PChar; MaxWidthInPixels : Longint;
  ProcessAmpersands : Boolean) : PChar;
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

    If ProcessAmpersands then begin
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

 { =============================================================================

  $Log$
  Revision 1.613  2005/01/07 20:51:11  micha
  swap TCustomStaticText and TCustomLabel

  Revision 1.612  2005/01/01 18:56:47  mattias
  implemented TTIProgressBar

  Revision 1.611  2004/12/18 23:46:16  mattias
  added resurce strings for gtk file dlg

  Revision 1.610  2004/11/28 00:55:44  mattias
  deactivated sending SYSKey messages in gtk intf - they are not used anyway

  Revision 1.609  2004/11/20 11:20:06  mattias
  implemented creating classes at run time from any TComponent descendant

  Revision 1.608  2004/11/08 19:11:55  mattias
  disabled hardly used gtk FillScreenFont, this should be only done on demand, improved getting default font family for gtk

  Revision 1.607  2004/11/03 14:18:36  mattias
  implemented preferred size for controls for theme depending AutoSizing

  Revision 1.606  2004/10/16 15:36:49  mattias
  implemented gtkwscomctrls.TGtkWSStatusBar

  Revision 1.605  2004/10/01 13:16:44  mattias
  fixed unselecting TCanvas objects

  Revision 1.604  2004/09/25 15:05:38  mattias
  implemented Rename Identifier

  Revision 1.603  2004/09/24 21:34:14  micha
  convert LM_CREATE message to interface methods
  remove SendMsgToInterface, CNSendMessage and related methods
  remove TWidgetSet.IntSendMessage3; all LCL to interface messages have been converted

  Revision 1.602  2004/09/24 19:02:38  micha
  convert LM_MOVEPAGE message to interface method

  Revision 1.601  2004/09/24 18:00:52  micha
  convert LM_NB_UPDATETAB message to interface method

  Revision 1.600  2004/09/24 17:20:43  micha
  convert LM_SETGEOMETRY message to interface method

  Revision 1.599  2004/09/24 15:31:01  micha
  convert LM_LB_GETTOPINDEX and LM_LB_SETTOPINDEX message to interface methods

  Revision 1.598  2004/09/24 14:50:57  micha
  convert LM_SETDESIGNING message to TWidgetSet method

  Revision 1.597  2004/09/24 07:52:35  micha
  convert LM_SETPROPERTIES message to interface method for TCustomTrackBar
  remove message LM_SETPROPERTIES, conversion done

  Revision 1.596  2004/09/23 21:16:45  micha
  convert LM_SETPROPERTIES message to interface method for TCustomSpinEdit

  Revision 1.595  2004/09/23 20:36:30  micha
  convert LM_SETPROPERTIES message to interface method for TScrollBar

  Revision 1.594  2004/09/23 14:50:47  micha
  convert LM_SETPROPERTIES message to interface method for TCustomProgressBar

  Revision 1.593  2004/09/22 18:06:32  micha
  convert LM_SETPROPERTIES message to interface methods for TCustomListView

  Revision 1.592  2004/09/22 16:13:01  micha
  convert LM_SETPROPERTIES message to interface methods for TCustomMemo

  Revision 1.591  2004/09/22 14:50:18  micha
  convert LM_SETPROPERTIES message for tcustomlabel to interface methods

  Revision 1.590  2004/09/21 13:28:10  micha
  convert LM_SETPROPERTIES to interface methods for TCustomEdit

  Revision 1.589  2004/09/21 10:05:26  mattias
  fixed disable at designtime and bounding TCustomProgressBar position

  Revision 1.588  2004/09/20 21:01:04  micha
  convert LM_SETPROPERTIES to interface methods for TCustomComboBox

  Revision 1.587  2004/09/19 19:39:10  micha
  undo removal of LM_SETDESIGNING; used by lazarus ide (main.pp)

  Revision 1.585  2004/09/19 18:50:28  micha
  convert LM_SETVALUE message to interface methods

  Revision 1.584  2004/09/18 17:07:57  micha
  convert LM_GETVALUE message to interface method

  Revision 1.583  2004/09/18 12:43:15  micha
  convert LM_DESTROY message to interface methods

  Revision 1.582  2004/09/18 11:06:47  micha
  remove LM_RECREATEWND message, as it is not used by LCL

  Revision 1.581  2004/09/18 10:52:48  micha
  convert LM_SCREENINIT message to interface method (integrated with TWidgetSet.AppInit(var ScreenInfo)

  Revision 1.580  2004/09/18 01:18:00  mattias
  removed unneeded handle

  Revision 1.579  2004/09/17 20:30:13  vincents
  replaced write by DbgOut

  Revision 1.578  2004/09/17 10:56:25  micha
  convert LM_SHORTCUT message to interface methods

  Revision 1.577  2004/09/17 07:55:13  micha
  convert LM_SETBORDER message to interface method
  fix widgetsets virtual methods to be published
  fix compilation debugging widgetset registration

  Revision 1.576  2004/09/16 14:32:31  micha
  convert LM_SETSELMODE message to interface method

  Revision 1.575  2004/09/16 13:57:30  micha
  convert LM_SETSEL message to interface method

  Revision 1.574  2004/09/16 13:30:48  micha
  convert LM_SORT message to interface method

  Revision 1.573  2004/09/15 19:38:55  micha
  convert LM_GETSEL message to interface method

  Revision 1.572  2004/09/15 19:04:39  micha
  convert LM_GETSELCOUNT message to interface method

  Revision 1.571  2004/09/15 18:50:33  micha
  remove LM_GETLINECOUNT message as it is not used by the LCL

  Revision 1.570  2004/09/15 17:21:22  micha
  convert LM_GETITEMINDEX and LM_SETITEMINDEX messages to interface methods

  Revision 1.569  2004/09/15 14:45:39  micha
  convert LM_GETITEMS message to interface method

  Revision 1.568  2004/09/15 07:57:59  micha
  convert LM_SETFORMICON message to interface method

  Revision 1.567  2004/09/14 15:48:28  micha
  convert LM_INVALIDATE message to interface method

  Revision 1.566  2004/09/14 14:41:17  micha
  convert LM_INSERTTOOLBUTTON and LM_DELETETOOLBUTTON messages to interface methods; warning: still very ugly code, as if it is "OldToolbar" so probably, obsolete

  Revision 1.565  2004/09/14 12:45:29  micha
  convert LM_SETTABPOSITION message to interface method

  Revision 1.564  2004/09/14 10:06:26  micha
  convert LM_REDRAW message to interface method (in twidgetset)

  Revision 1.563  2004/09/13 19:57:30  micha
  convert LM_SHOWTABS message to interface method

  Revision 1.562  2004/09/13 19:06:04  micha
  convert LM_ADDPAGE and LM_REMOVEPAGE messages to new interface methods

  Revision 1.561  2004/09/13 14:34:53  micha
  convert LM_TB_BUTTONCOUNT to interface method

  Revision 1.560  2004/09/13 13:13:46  micha
  convert LM_SHOWMODAL to interface methods

  Revision 1.559  2004/09/12 19:50:35  micha
  convert LM_SETSIZE message to new interface method

  Revision 1.558  2004/09/12 18:56:36  mazen
  * Fix compilation problem with GTK2

  Revision 1.557  2004/09/12 13:30:13  micha
  remove handling of LM_SETFOCUS in interface, as it is never sent from LCL

  Revision 1.556  2004/09/12 13:21:37  micha
  remove obsolete message LM_DRAGINFOCHANGED

  Revision 1.555  2004/09/12 13:11:50  micha
  convert LM_GETPIXEL and LM_SETPIXEL to interface methods (of twidgetset, DCGetPixel and DCSetPixel)

  Revision 1.554  2004/09/11 17:29:10  micha
  convert LM_POPUPSHOW message to interface method

  Revision 1.553  2004/09/11 15:01:22  micha
  remove obsolete LM_SETFILTER and LM_SETFILENAME messages

  Revision 1.552  2004/09/11 14:54:01  micha
  convert LM_BTNDEFAULT_CHANGED message to interface method

  Revision 1.551  2004/09/11 13:38:37  micha
  convert LM_BRINGTOFRONT message to interface method
  NOTE: was only used for tapplication, not from other controls

  Revision 1.550  2004/09/11 13:06:48  micha
  convert LM_ADDCHILD message to interface method

  Revision 1.549  2004/09/10 20:19:13  micha
  convert LM_CLB_G/SETCHECKED to interface methods

  Revision 1.548  2004/09/10 18:58:22  micha
  convert LM_ATTACHMENU to interface method

  Revision 1.547  2004/09/10 17:59:58  micha
  convert LM_APPENDTEXT to interface method

  Revision 1.546  2004/09/10 16:28:51  mattias
  implemented very rudimentary TTabControl

  Revision 1.545  2004/09/10 14:38:29  micha
  convert lm_gettext to new interface methods
  remove lm_settext replacement settext methods in twidgetsets

  Revision 1.544  2004/09/10 11:20:44  micha
  remove LM_SETTEXT message as it is not used

  Revision 1.543  2004/09/10 09:43:13  micha
  convert LM_SETLABEL message to interface methods

  Revision 1.542  2004/09/08 20:47:17  micha
  convert LM_SHOWHIDE message to new intf method TWSWinControl.ShowHide

  Revision 1.541  2004/09/08 19:09:34  micha
  convert LM_SETCOLOR message to new intf method TWSWinControl.SetColor

  Revision 1.540  2004/09/07 10:26:16  micha
  fix logs to get rid of comment level 2 warning

  Revision 1.539  2004/09/07 09:44:46  micha
  convert lcl messages to new interface using methods: LM_G/SETSELSTART, LM_G/SETSELLEN, LM_G/SETLIMITTEXT

  Revision 1.538  2004/09/06 22:24:52  mattias
  started the carbon LCL interface

  Revision 1.537  2004/09/02 09:17:00  mattias
  improved double byte char fonts for gtk1, started synedit UTF8 support

  Revision 1.536  2004/08/30 10:49:20  mattias
  fixed focus catch for combobox csDropDownList

  Revision 1.535  2004/08/28 10:22:13  mattias
  added hints for long props in OI  from Andrew Haines

  Revision 1.534  2004/08/27 08:55:22  micha
  implement tapplication.minimize for win32, stub for gtk

  Revision 1.533  2004/08/25 18:30:05  micha
  remove obsolete message handlers

  Revision 1.532  2004/08/25 16:11:06  micha
  remove obsolete message handlers

  Revision 1.531  2004/08/23 15:05:09  mattias
  implemented help jump to FPDoc html unit

  Revision 1.530  2004/08/19 18:50:53  mattias
  splitted IDE component owner hierachy to reduce notification time

  Revision 1.529  2004/08/18 20:49:02  mattias
  simple forms can now be child controls

  Revision 1.528  2004/08/18 09:08:34  mattias
  fixed deleting of collection item in prop editor

  Revision 1.527  2004/08/17 19:01:36  mattias
  gtk intf now ignores size notifications of unrealized widgets

  Revision 1.526  2004/08/13 21:46:31  mattias
  added TLazIntfImage.SetColor_BPP32_R8G8B8_A1_BIO_TTB_RBO

  Revision 1.525  2004/08/13 20:40:27  mattias
  fixed DebugLn for VerboseRawImage

  Revision 1.524  2004/08/13 12:52:58  mattias
  removed IFDEF unused var

  Revision 1.523  2004/08/13 12:41:54  mattias
  fixed uninitialized argument

  Revision 1.522  2004/08/11 12:57:03  mattias
  improved gtk1 FontCache to handle several descriptors per gdkfont

  Revision 1.521  2004/08/10 17:34:13  mattias
  implemented font cache for gtk, which accelerates switching fonts

  Revision 1.520  2004/08/09 21:12:43  mattias
  implemented FormStyle fsSplash for splash screens

  Revision 1.519  2004/08/09 18:22:08  mattias
  fixed type of Low(open array) and type of PathDelim

  Revision 1.518  2004/07/30 20:55:16  vincents
  updated localization

  Revision 1.517  2004/07/30 14:20:29  mazen
  - space style and space size are not set by program in GTK2 but by theme

  Revision 1.516  2004/07/23 16:44:27  mattias
  activated new TToolbar, old can be activated with -dOldToolBar

  Revision 1.515  2004/07/16 21:49:00  mattias
  added RTTI controls

  Revision 1.514  2004/07/15 10:43:39  mattias
  added TCustomButton, TCustomBitBtn, TCustomSpeedButton

  Revision 1.513  2004/07/11 17:20:47  marc
  * Implemented most of TListColoum/Item in the Ws for gtk and win32

  Revision 1.512  2004/07/05 15:48:31  mazen
  * fix compilation error with gtk2 platform

  Revision 1.511  2004/06/28 17:03:37  mattias
  clean up

  Revision 1.510  2004/06/28 08:54:20  mattias
  fixed ord ptr conversion hints

  Revision 1.509  2004/06/22 23:55:51  marc
  * Fixed RawImage.Description.AlphaLineEnd (it is not always dword based)

  Revision 1.508  2004/06/19 00:06:47  mattias
  fixed compilation

  Revision 1.507  2004/05/31 08:21:52  mattias
  fsStayOnTop is now ignored in design mode

  Revision 1.506  2004/05/30 14:02:31  mattias
  implemented OnChange for TRadioButton, TCheckBox, TToggleBox and some more docking stuff

  Revision 1.505  2004/05/22 14:59:23  mattias
  fixed multi update of TStatusBar

  Revision 1.504  2004/05/22 14:35:32  mattias
  fixed button return key

  Revision 1.503  2004/05/18 23:10:41  marc
  * Started to move TListview to the WS interface

  Revision 1.502  2004/05/16 23:24:41  marc
  + Added WSBitBtn interface
  + Implemented WSBitBtn interface for gtk

  Revision 1.501  2004/05/14 12:53:25  mattias
  improved grids e.g. OnPrepareCanvas   patch from Jesus

  Revision 1.500  2004/05/11 12:16:47  mattias
  replaced writeln by debugln

  Revision 1.499  2004/05/11 09:49:47  mattias
  started sending CN_KEYUP

  Revision 1.498  2004/05/01 10:23:04  mattias
  fixed progessbar position

  Revision 1.497  2004/04/19 10:06:56  mattias
  fixed illegal ancestor search

  Revision 1.496  2004/04/19 09:30:04  marc
  * Fixed compilation for gtk2

  Revision 1.495  2004/04/18 23:55:39  marc
  * Applied patch from Ladislav Michl
  * Changed the way TControl.Text is resolved
  * Added setting of text to TWSWinControl

  Revision 1.494  2004/04/11 18:58:25  micha
  fix (lm_)setcursor changes for gtk target

  Revision 1.493  2004/04/09 22:59:09  mattias
  fixed mem leak in CreateFilter menu items for file dialog

  Revision 1.492  2004/04/08 18:27:51  mattias
  fixed memleak in TDefaultComponentEditor.Edit

  Revision 1.491  2004/04/05 11:41:06  mattias
  fixed retrieving gdkbitmaps  LineEnding=rileDWordBoundary

  Revision 1.490  2004/04/04 17:10:05  marc
  Patch from Andrew Haines

  Revision 1.489  2004/04/03 18:08:39  mattias
  fixed TLabel.AutoWrap=true and label on formless parent in gtk intf

  Revision 1.488  2004/04/03 16:47:46  mattias
  implemented converting gdkbitmap to RawImage mask

  Revision 1.487  2004/04/03 12:51:17  mattias
  fixed shrinking forms under gtk  from vincent

  Revision 1.486  2004/04/02 20:44:08  mattias
  fixed LM_LV_AddItem message in gtk intf from  Andrew H.

  Revision 1.485  2004/03/28 12:49:22  mattias
  implemented mask merge and extraction for raw images

  Revision 1.484  2004/03/24 01:21:41  marc
  * Simplified signals for gtkwsbutton

  Revision 1.483  2004/03/22 19:10:04  mattias
  implemented icons for TPage in gtk, mask for TCustomImageList

  Revision 1.482  2004/03/19 00:03:15  marc
  * Moved the implementation of (GTK)ButtonCreateHandle to the new
    (GTK)WSButton class

  Revision 1.481  2004/03/18 22:35:53  mattias
  improved TCustomListView.ItemAdded with an Index param  from Andrew

  Revision 1.480  2004/03/18 00:55:56  mattias
  fixed memleak in gtk opendlg

  Revision 1.479  2004/03/09 15:30:15  peter
    * fixed gtk2 compilation

  Revision 1.478  2004/03/06 21:57:14  mattias
  fixed compilation under fpc 1.9.3

  Revision 1.477  2004/03/06 17:12:19  mattias
  fixed CreateBrushIndirect

  Revision 1.476  2004/03/06 15:37:43  mattias
  fixed FreeDC

  Revision 1.475  2004/03/05 00:31:52  marc
  * Renamed TGtkObject to TGtkWidgetSet

  Revision 1.474  2004/02/28 00:34:35  mattias
  fixed CreateComponent for buttons, implemented basic Drag And Drop

  Revision 1.473  2004/02/27 00:42:41  marc
  * Interface CreateComponent splitup
  * Implemented CreateButtonHandle on GTK interface
    on win32 interface it still needs to be done
  * Changed ApiWizz to support multilines and more interfaces

  Revision 1.472  2004/02/23 18:24:38  mattias
  completed new TToolBar

  Revision 1.471  2004/02/22 10:43:20  mattias
  added child-parent checks

  Revision 1.470  2004/02/21 15:37:33  mattias
  moved compiler options to project menu, added -CX for smartlinking

  Revision 1.469  2004/02/21 01:01:03  mattias
  added uninstall popupmenuitem to package graph explorer

  Revision 1.468  2004/02/13 15:49:54  mattias
  started advanced LCL auto sizing

  Revision 1.467  2004/02/12 18:09:10  mattias
  removed win32 specific TToolBar code in new TToolBar, implemented TWinControl.FlipChildren

  Revision 1.466  2004/02/11 11:34:16  mattias
  started new TToolBar

  Revision 1.465  2004/02/10 00:05:03  mattias
  TSpeedButton now uses MaskBlt

  Revision 1.464  2004/02/07 18:04:14  mattias
  fixed grids OnDrawCells

  Revision 1.463  2004/02/05 16:28:38  mattias
  fixed unsharing TBitmap

  Revision 1.462  2004/02/04 00:04:37  mattias
  added some TEdit ideas to TSpinEdit

  Revision 1.461  2004/02/03 20:01:29  mattias
  fixed gtk intf WaitMessages

  Revision 1.460  2004/02/02 19:48:01  mattias
  fixed removing TStatusBar panels in gtk

  Revision 1.459  2004/02/02 12:44:45  mattias
  implemented interface constraints

  Revision 1.458  2004/02/02 00:41:06  mattias
  TScrollBar now automatically checks Align and Anchors for useful values

  Revision 1.457  2004/01/27 21:32:11  mattias
  improved changing style of controls

  Revision 1.456  2004/01/27 10:09:44  mattias
  fixed renaming of DFM to LFM

  Revision 1.455  2004/01/23 13:55:30  mattias
  style widgets are now realized, so all values are initialized

  Revision 1.454  2004/01/22 11:23:36  mattias
  started MaskBlt for gtkIF and applied patch for dir dlg in env opts from Vincent

  Revision 1.453  2004/01/14 20:09:50  mattias
  added TColorDialog debugging

  Revision 1.452  2004/01/13 16:39:02  mattias
  changed consistency stops during var renaming to errors

  Revision 1.451  2004/01/12 23:56:10  mattias
  improved double buffering, only one issue left: parent gdkwindow paint messages

  Revision 1.450  2004/01/12 13:43:12  mattias
  improved and activated new statusbar

  Revision 1.449  2004/01/10 22:34:20  mattias
  started double buffering for gtk intf

  Revision 1.448  2004/01/10 00:46:46  mattias
  fixed DestroyComponent

  Revision 1.447  2004/01/09 20:03:13  mattias
  implemented new statusbar methods in gtk intf

  Revision 1.446  2004/01/06 17:58:06  mattias
  fixed setting TRadioButton.Caption for gtk

  Revision 1.445  2004/01/06 15:20:19  mattias
  fixed instant termination of gtk message handling

  Revision 1.444  2004/01/03 11:57:48  mattias
  applied implementation for LM_LB_GETINDEXAT  from Vincent

  Revision 1.443  2003/12/25 14:17:07  mattias
  fixed many range check warnings

  Revision 1.442  2003/12/23 11:16:41  mattias
  started key combinations, fixed some range check errors

  Revision 1.441  2003/12/16 14:01:27  mattias
  fixed compilation gtk and fpc 1.9

  Revision 1.440  2003/11/30 18:35:19  mattias
  fixed fpc 1.9.1 warns

  Revision 1.439  2003/11/29 13:17:38  mattias
  made gtklayout using window theme at start

  Revision 1.438  2003/11/28 11:25:49  mattias
  added BitOrder for RawImages

  Revision 1.437  2003/11/26 21:30:19  mattias
  reduced unit circles, fixed fpImage streaming

  Revision 1.436  2003/11/08 14:12:48  mattias
  fixed scrollbar events under gtk from Colin

  Revision 1.435  2003/11/03 22:37:41  mattias
  fixed vert scrollbar, implemented GetDesignerDC

  Revision 1.434  2003/11/01 10:27:41  mattias
  fpc 1.1 fixes, started scrollbar hiding, started polymorphing client areas

  Revision 1.433  2003/10/16 23:54:27  marc
  Implemented new gtk keyevent handling

  Revision 1.432  2003/10/06 16:13:52  ajgenius
  partly fixed gtk2 mouse offsets;
  added new includes to gtk2 lpk

  Revision 1.431  2003/10/06 10:50:10  mattias
  added recursion to InvalidateClientRectCache

  Revision 1.430  2003/10/02 01:18:38  ajgenius
  more callbacks fixes for gtk2, partly fix gtk2 CheckListBox

  Revision 1.429  2003/09/25 20:44:42  ajgenius
  minor changes for gtk2

  Revision 1.428  2003/09/25 16:02:16  ajgenius
  try to catch GDK/X drawable errors and raise an AV to stop killing App

  Revision 1.427  2003/09/24 17:23:54  ajgenius
  more work toward GTK2 - partly fix CheckListBox, & MenuItems

  Revision 1.426  2003/09/23 17:52:04  mattias
  added SetAnchors

  Revision 1.425  2003/09/23 08:00:46  mattias
  improved OnEnter for gtkcombo

  Revision 1.424  2003/09/22 20:08:56  ajgenius
  break GTK2 object and winapi into includes like the GTK interface

  Revision 1.423  2003/09/22 19:17:26  ajgenius
  begin implementing GtkTreeView for ListBox/CListBox

  Revision 1.422  2003/09/22 15:34:07  ajgenius
  use GtkImage and Pixbuf for GTK2 instead of Deprecated GtkPixmap

  Revision 1.421  2003/09/20 13:27:49  mattias
  varois improvements for ParentColor from Micha

  Revision 1.420  2003/09/19 00:41:51  ajgenius
  remove USE_PANGO define since pango now apears to work properly.

  Revision 1.419  2003/09/18 21:36:00  ajgenius
  add csEdit to GTK2 interface to start removing use of GtkOldEditable

  Revision 1.418  2003/09/18 12:15:01  mattias
  fixed is checks for TCustomXXX controls

  Revision 1.417  2003/09/17 19:40:46  ajgenius
  Initial DoubleBuffering Support for GTK2

  Revision 1.416  2003/09/17 15:26:41  mattias
  fixed removing TCustomPage

  Revision 1.415  2003/09/12 17:40:45  ajgenius
  fixes for GTK2(accel groups, menu accel, 'draw'),
  more work toward Pango(DrawText now works, UpdateDCTextMetric mostly works)

  Revision 1.414  2003/09/11 21:33:11  ajgenius
  partly fixed TWinControl(csFixed)

  Revision 1.413  2003/09/10 18:03:46  ajgenius
  more changes for pango -
  partly fixed ref counting,
  added Pango versions of TextOut, CreateFontIndirectEx, and GetTextExtentPoint to the GTK2 interface

  Revision 1.412  2003/09/10 02:33:41  ajgenius
  fixed TColotDialog for GTK2

  Revision 1.411  2003/09/09 20:46:38  ajgenius
  more implementation toward pango for gtk2

  Revision 1.410  2003/09/09 04:15:08  ajgenius
  more updates for GTK2, more GTK1 wrappers, removal of more ifdef's, partly fixed signals

  Revision 1.409  2003/09/06 20:23:53  ajgenius
  fixes for gtk2
  added more wrappers for gtk1/gtk2 converstion and sanity
  removed pointless version $Ifdef GTK2 etc
  IDE now "runs" Tcontrol drawing/using problems
  renders it unuseable however

  Revision 1.408  2003/09/06 17:24:52  ajgenius
  gtk2 changes for pixmap, getcursorpos, mouse events workaround

  Revision 1.407  2003/09/05 19:29:38  mattias
  Success: The first gtk2 application ran without error

  Revision 1.406  2003/09/05 18:19:54  ajgenius
  Make GTK2 "compile". linking fails still
  (Makefile.fpc needs pkgconfig libs/GTK2 linking rules,
  but not sure how not sure how) and when linked via a make script
  (like gtk2 examples do) apps still won't work(yet).  I think we
  need to do a lot of work to make sure incompatible(also to get rid
  of deprecated) things are done in GTK2 interface itself, and just
  use more $Ifdef GTK1 in the gtk interface itself.

  Revision 1.405  2003/09/04 11:10:18  mattias
  added csClickEvents to TImage

  Revision 1.404  2003/09/04 10:51:30  mattias
  fixed default size of preview widget

  Revision 1.403  2003/09/02 21:32:56  mattias
  implemented TOpenPictureDialog

  Revision 1.402  2003/08/30 18:53:07  mattias
  using default colors, when theme does not define them

  Revision 1.401  2003/08/29 21:21:07  mattias
  fixes for gtk2

  Revision 1.400  2003/08/28 09:10:00  mattias
  listbox and comboboxes now set sort and selection at handle creation

  Revision 1.399  2003/08/18 13:21:23  mattias
  renamed lazqueue to lazlinkedlist, patch from Jeroen

  Revision 1.398  2003/08/15 14:01:20  mattias
  combined lazconf things for unix

  Revision 1.397  2003/08/14 10:36:55  mattias
  added TSelectDirectoryDialog

  Revision 1.396  2003/08/13 00:02:06  marc
  + introduced interface exceptions
  - Removed ifdefs for implemented gtkwin32 functions

  Revision 1.395  2003/07/25 08:00:36  mattias
  fixed sending follow up move/size messages from gtk

  Revision 1.394  2003/07/23 10:23:56  mattias
  started README about remote debugging

  Revision 1.393  2003/07/07 23:58:43  marc
  + Implemented TCheckListBox.Checked[] property

  Revision 1.392  2003/07/06 17:53:34  mattias
  updated polish localization

  Revision 1.391  2003/07/04 22:06:49  mattias
  implemented interface graphics

  Revision 1.390  2003/07/04 08:54:53  mattias
  implemented 16bit rawimages for gtk

  Revision 1.389  2003/07/03 18:10:55  mattias
  added fontdialog options to win32 intf from Wojciech Malinowski

  Revision 1.388  2003/07/02 15:56:15  mattias
  fixed win32 painting and started creating bitmaps from rawimages

  Revision 1.387  2003/07/02 10:02:51  mattias
  fixed TPaintStruct

  Revision 1.386  2003/06/30 10:09:46  mattias
  fixed Get/SetPixel for DC without widget

  Revision 1.385  2003/06/28 12:10:02  mattias
  fixed LM_SETSIZE in InitializeWnd

  Revision 1.384  2003/06/27 23:42:38  mattias
  fixed TScrollBar resizing

  Revision 1.383  2003/06/26 18:18:25  mattias
  fixed recaching

  Revision 1.382  2003/06/26 17:00:00  mattias
  fixed result on searching proc in interface

  Revision 1.381  2002/08/19 15:15:24  mattias
  implemented TPairSplitter

  Revision 1.380  2002/08/18 16:50:09  mattias
  fixes for debugging

  Revision 1.379  2002/08/18 04:57:01  mattias
  fixed csDashDot

  Revision 1.378  2002/08/17 23:41:34  mattias
  many clipping fixes

  Revision 1.377  2003/06/20 01:37:47  marc
  + Added TCheckListBox component

  Revision 1.376  2003/06/13 10:09:04  mattias
  fixed Set/GetPixel

  Revision 1.375  2003/06/12 16:18:23  mattias
  applied TComboBox fix for grabbing keys from Yoyong

  Revision 1.374  2003/06/10 17:23:35  mattias
  implemented tabstop

  Revision 1.373  2003/06/10 12:28:23  mattias
  fixed anchoring controls

  Revision 1.372  2003/06/09 14:39:52  mattias
  implemented setting working directory for debugger

  Revision 1.371  2003/06/09 10:07:34  mattias
  updated russian localization from Vasily

  Revision 1.370  2003/06/09 09:31:40  mattias
  fixed 1_0_8 difference

  Revision 1.369  2003/06/09 09:20:27  mattias
  removed menubar.inc

  Revision 1.368  2003/06/07 09:34:21  mattias
  added ambigius compiled unit test for packages

  Revision 1.367  2003/06/03 10:29:22  mattias
  implemented updates between source marks and breakpoints

  Revision 1.366  2003/05/30 16:25:47  mattias
  started datamodule

  Revision 1.365  2003/05/20 21:41:07  mattias
  started loading/saving breakpoints

  Revision 1.364  2003/05/19 08:16:33  mattias
  fixed allocation of dc backcolor

  Revision 1.363  2003/05/18 10:42:58  mattias
  implemented deleting empty submenus

  Revision 1.362  2003/05/14 13:06:00  mattias
  fixed setting TListBox.Selected before createhandle

  Revision 1.361  2003/05/01 11:44:03  mattias
  fixed changing menuitem separator and normal

  Revision 1.360  2003/04/29 19:00:43  mattias
  added package gtkopengl

  Revision 1.359  2003/04/29 13:35:39  mattias
  improved configure build lazarus dialog

  Revision 1.358  2003/04/26 10:45:34  mattias
  fixed right control release

  Revision 1.357  2003/04/26 07:34:55  mattias
  implemented custom package initialization

  Revision 1.356  2003/04/25 14:40:49  mattias
  implemented add file to a package dialog

  Revision 1.355  2003/04/20 20:32:40  mattias
  implemented removing, re-adding, updating project dependencies

  Revision 1.354  2003/04/15 09:03:46  mattias
  reduced output

  Revision 1.353  2003/04/15 08:54:27  mattias
  fixed TMemo.WordWrap

  Revision 1.352  2003/04/11 17:10:20  mattias
  added but not implemented ComboBoxDropDown

  Revision 1.351  2003/04/11 12:48:07  mattias
  fixed gtk warning on setting item height

  Revision 1.350  2003/04/10 09:22:42  mattias
  implemented changing dependency version

  Revision 1.349  2003/04/08 00:09:03  mattias
  added LM_APPENDTEXT from hernan

  Revision 1.348  2003/04/04 16:35:24  mattias
  started package registration

  Revision 1.347  2003/04/03 17:42:13  mattias
  added exception handling for createpixmapindirect

  Revision 1.346  2003/04/02 13:23:24  mattias
  fixed default font

  Revision 1.345  2003/03/29 19:15:30  mattias
  fixed untransienting

  Revision 1.344  2003/03/27 22:16:01  mattias
  implemented findeclaration gdb exceptions

  Revision 1.343  2003/03/26 19:25:27  mattias
  added transient deactivation option and updated localization

  Revision 1.342  2003/03/26 00:21:25  mattias
  implemented build lazarus extra options -d

  Revision 1.341  2003/03/25 16:29:53  mattias
  fixed sending TButtonControl.OnClick on every change

  Revision 1.340  2003/03/25 13:00:39  mattias
  implemented TMemo.SelLength, improved OI hints

  Revision 1.339  2003/03/25 10:45:41  mattias
  reduced focus handling and improved focus setting

  Revision 1.338  2003/03/18 13:45:39  mattias
  set transient forms with Screen object order

  Revision 1.337  2003/03/18 13:04:25  mattias
  improved focus debugging output

  Revision 1.336  2003/03/17 20:53:16  mattias
  removed SetRadioButtonGroupMode

  Revision 1.335  2003/03/17 20:50:30  mattias
  fixed TRadioGroup.ItemIndex=-1

  Revision 1.334  2003/03/17 13:54:34  mattias
  fixed setting activecontrol after createwnd

  Revision 1.333  2003/03/17 13:00:35  mattias
  improved but not fixed transient windows

  Revision 1.332  2003/03/15 18:32:38  mattias
  implemented transient windows for all cases

  Revision 1.331  2003/03/15 09:42:50  mattias
  fixed transient windows

  Revision 1.330  2003/03/11 23:14:19  mattias
  added TControl.HandleObjectShouldBeVisible

  Revision 1.329  2003/03/11 22:56:41  mattias
  added visiblechanging

  Revision 1.328  2003/03/09 21:13:32  mattias
  localized gtk interface

  Revision 1.327  2003/03/09 17:44:12  mattias
  finshed Make Resourcestring dialog and implemented TToggleBox

  Revision 1.326  2003/02/28 19:10:25  mattias
  added new ... dialog

  Revision 1.325  2003/02/24 22:47:28  mattias
  fixed setting TTreeView.ScrollBars

  Revision 1.324  2003/02/24 11:51:44  mattias
  combobox height can now be set, added OI item height option

  Revision 1.323  2003/02/23 10:42:06  mattias
  implemented changing TMenuItem.GroupIndex at runtime

  Revision 1.322  2003/02/05 13:46:57  mattias
  fixed TCustomEdit.SelStart when nothing selected

  Revision 1.321  2003/01/18 21:31:43  mattias
  fixed scrolling offset of TScrollingWinControl

  Revision 1.320  2003/01/18 19:03:38  mattias
  fixed TSpinEdit.Value

  Revision 1.319  2003/01/06 12:00:16  mattias
  implemented fsStayOnTop+bsNone for forms under gtk (useful for splash)

  Revision 1.318  2003/01/01 13:01:01  mattias
  fixed setcolor for streamed components

  Revision 1.317  2003/01/01 12:38:53  mattias
  clean ups

  Revision 1.316  2003/01/01 10:46:59  mattias
  fixes for win32 listbox/combobox from Karl Brandt

  Revision 1.315  2002/12/29 18:13:38  mattias
  identifier completion: basically working, still hidden

  Revision 1.314  2002/12/28 21:44:51  mattias
  further cleanup

  Revision 1.313  2002/12/28 12:42:38  mattias
  focus fixes, reduced lpi size

  Revision 1.312  2002/12/28 11:29:47  mattias
  xmlcfg deletion, focus fixes

  Revision 1.311  2002/12/27 17:46:04  mattias
  fixed SetColor

  Revision 1.310  2002/12/27 17:12:38  mattias
  added more Delphi win32 compatibility functions

  Revision 1.309  2002/12/27 10:23:40  mattias
  implemented TListBox.TopIndex

  Revision 1.308  2002/12/27 09:05:50  mattias
  fixed uninitialized logbrush

  Revision 1.307  2002/12/27 08:46:32  mattias
  changes for fpc 1.1

  Revision 1.306  2002/12/26 11:00:14  mattias
  added included by to unitinfo and a few win32 functions

  Revision 1.305  2002/12/25 14:21:28  mattias
  fixed setting activecontrol to nil when removing component

  Revision 1.304  2002/12/25 11:53:47  mattias
  Button.Default now sets focus

  Revision 1.303  2002/12/22 23:07:28  mattias
  mem leak with tooltips, fix from Jeroen

  Revision 1.302  2002/12/22 22:42:55  mattias
  custom controls now support child wincontrols

  Revision 1.301  2002/12/17 16:32:12  mattias
  freeing GDIObjects without AppTerminate

  Revision 1.300  2002/12/16 12:12:50  mattias
  fixes for fpc 1.1

  Revision 1.299  2002/02/09 02:30:56  mattias
  added patch from Jeroen van Idekinge

  Revision 1.298  2002/02/09 01:48:23  mattias
  renamed TinterfaceObject.Init to AppInit and TWinControls can now contain childs in gtk

  Revision 1.297  2002/12/12 17:47:46  mattias
  new constants for compatibility

  Revision 1.296  2002/12/07 08:42:08  mattias
  improved ExtTxtOut: support for char dist array

  Revision 1.295  2002/12/05 22:16:30  mattias
  double byte char font started

  Revision 1.294  2002/12/04 20:39:15  mattias
  patch from Vincent: clean ups and fixed crash on destroying window

  Revision 1.293  2002/11/27 15:40:36  mattias
  fixed resize request

  Revision 1.292  2002/11/23 13:48:44  mattias
  added Timer patch from Vincent Snijders

  Revision 1.291  2002/11/21 18:49:53  mattias
  started OnMouseEnter and OnMouseLeave

  Revision 1.290  2002/11/18 13:56:33  mattias
  fixed TListView.Items.Add

  Revision 1.289  2002/11/18 13:38:44  mattias
  fixed buffer overrun and added several checks

  Revision 1.288  2002/11/16 11:22:57  mbukovjan
  Fixes to MaxLength. TCustomMemo now has MaxLength, too.

  Revision 1.287  2002/11/15 23:52:06  mbukovjan
  Fix keydown & keypress for TMemo and hopefully not break others.

  Revision 1.286  2002/11/13 23:03:05  lazarus
  MG: improved warning

  Revision 1.285  2002/11/13 08:40:44  lazarus
  MB: Fixed selection start/end/text for edits and combos. Add support for memos.

  Revision 1.284  2002/11/12 13:16:05  lazarus
  MG: fixed TListView with more than 2 columns

  Revision 1.283  2002/11/12 10:53:44  lazarus
  MG: fixed setting gdk pen style

  Revision 1.282  2002/11/12 10:16:18  lazarus
  MG: fixed TMainMenu creation

  Revision 1.281  2002/11/09 18:13:33  lazarus
  MG: fixed gdkwindow checks

  Revision 1.280  2002/11/09 15:02:07  lazarus
  MG: fixed LM_LVChangedItem, OnShowHint, small bugs

  Revision 1.279  2002/11/06 17:46:36  lazarus
  MG: reduced showing forms during creation

  Revision 1.278  2002/11/04 19:49:36  lazarus
  MG: added persistent hints for main ide bar

  Revision 1.277  2002/11/03 20:53:37  lazarus
  MG: fixed typo

  Revision 1.276  2002/11/02 22:25:36  lazarus
  MG: implemented TMethodList and Application Idle handlers

  Revision 1.275  2002/10/31 18:54:17  lazarus
  MG: fixed loop

  Revision 1.274  2002/10/30 17:43:35  lazarus
  AJ: added IsNullBrush checks to reduce pointless color allocations & GDK function calls

  Revision 1.273  2002/10/30 13:50:26  lazarus
  MG: fixed message without handle

  Revision 1.272  2002/10/30 13:20:11  lazarus
  MG: fixed example

  Revision 1.271  2002/10/30 12:37:25  lazarus
  MG: mouse cursors are now allocated on demand

  Revision 1.270  2002/10/30 00:08:09  lazarus
  MG: finished ParseRCFile

  Revision 1.269  2002/10/28 18:17:03  lazarus
  MG: impoved focussing, unfocussing on destroy and fixed unit search

  Revision 1.268  2002/10/26 15:15:51  lazarus
  MG: broke LCL<->interface circles

  Revision 1.267  2002/10/26 12:32:29  lazarus
  AJ:Minor fixes for Win32 GTK compiling

  Revision 1.266  2002/10/25 15:27:03  lazarus
  AJ: Moved form contents creation to gtkproc for code
      reuse between GNOME and GTK, and to make GNOME MDI
      programming easier later on.

  Revision 1.265  2002/10/24 22:10:39  lazarus
  AJ: More changes for better code reuse between gnome & gtk interfaces

  Revision 1.264  2002/10/24 08:56:30  lazarus
  MG: fixed TnoteBook AddPage and double creation of MeinMenu

  Revision 1.263  2002/10/24 06:12:43  lazarus
  MG: minor cleanups

  Revision 1.262  2002/10/23 20:47:27  lazarus
  AJ: Started Form Scrolling
      Started StaticText FocusControl
      Fixed Misc Dialog Problems
      Added TApplication.Title

  Revision 1.261  2002/10/22 18:54:56  lazarus
  MG: fixed menu streaming

  Revision 1.260  2002/10/22 12:12:08  lazarus
  MG: accelerators are now shared between non modal forms

  Revision 1.259  2002/10/21 22:12:47  lazarus
  MG: fixed frmactivate

  Revision 1.258  2002/10/21 03:23:35  lazarus
  AJ: rearranged GTK init stuff for proper GNOME init & less duplication between interfaces

  Revision 1.257  2002/10/20 21:54:03  lazarus
  MG: fixes for 1.1

  Revision 1.256  2002/10/20 21:49:10  lazarus
  MG: fixes for fpc1.1

  Revision 1.255  2002/10/20 19:03:56  lazarus
  AJ: minor fixes for FPC 1.1

  Revision 1.254  2002/10/18 16:08:10  lazarus
  AJ: Partial HintWindow Fix; Added Screen.Font & Font.Name PropEditor; Started to fix ComboBox DropDown size/pos

  Revision 1.253  2002/10/17 21:00:17  lazarus
  MG: fixed uncapturing of mouse

  Revision 1.252  2002/10/17 15:09:32  lazarus
  MG: made mouse capturing more strict

  Revision 1.251  2002/10/15 22:28:05  lazarus
  AJ: added forcelinebreaks

  Revision 1.250  2002/10/15 16:01:36  lazarus
  MG: fixed timers

  Revision 1.249  2002/10/15 14:18:29  lazarus
  MG: added TGtkObject.WordWrap

  Revision 1.248  2002/10/15 07:01:29  lazarus
  MG: fixed timer checking

  Revision 1.247  2002/10/14 19:00:49  lazarus
  MG: fixed zombie timers

  Revision 1.246  2002/10/11 07:28:05  lazarus
  MG: gtk interface now sends keyboard events via DeliverMessage

  Revision 1.245  2002/10/10 19:43:16  lazarus
  MG: accelerated GetTextMetrics

  Revision 1.244  2002/10/10 09:44:30  lazarus
  MG: fixed gtk warnings on creating TMemo

  Revision 1.243  2002/10/10 08:51:13  lazarus
  MG: added paint messages for some gtk internal widgets

  Revision 1.242  2002/10/09 11:46:05  lazarus
  MG: fixed loading TListView from stream

  Revision 1.241  2002/10/08 22:32:28  lazarus
  MG: fixed cool little bug (menu double attaching bug)

  Revision 1.240  2002/10/08 17:51:41  lazarus
  MG: fixed settings negative widget sizes

  Revision 1.239  2002/10/08 16:15:44  lazarus
  MG: fixed small typos and accelerated TDynHashArray.Contains

  Revision 1.238  2002/10/08 14:10:02  lazarus
  MG: added TDeviceContext.SelectedColors

  Revision 1.237  2002/10/08 13:42:23  lazarus
  MG: added TDevContextColorType

  Revision 1.236  2002/10/08 10:08:46  lazarus
  MG: accelerated GDIColor allocating

  Revision 1.235  2002/10/07 20:50:58  lazarus
  MG: accelerated SelectGDKPenProps

  Revision 1.234  2002/10/06 20:24:27  lazarus
  MG: fixed stopping keypress event if handled by LCL

  Revision 1.233  2002/10/06 17:55:45  lazarus
  MG: JITForms now sets csDesigning before creation

  Revision 1.232  2002/10/05 10:37:21  lazarus
  MG: fixed TComboBox.ItemIndex on CreateWnd

  Revision 1.231  2002/10/04 20:46:52  lazarus
  MG: improved TComboBox.SetItemIndex

  Revision 1.230  2002/10/04 14:24:15  lazarus
  MG: added DrawItem to TComboBox/TListBox

  Revision 1.229  2002/10/04 07:28:14  lazarus
  MG: fixed showmodal without Application.MainForm

  Revision 1.228  2002/10/03 14:47:31  lazarus
  MG: added TComboBox.OnPopup+OnCloseUp+ItemWidth

  Revision 1.227  2002/10/03 00:08:50  lazarus
  AJ: TCustomLabel Autosize, TCustomCheckbox '&' shortcuts started

  Revision 1.226  2002/10/01 10:12:34  lazarus
  MG: added SendCachedLCLMessages to interfacebase for wysiwyg

  Revision 1.225  2002/10/01 10:05:48  lazarus
  MG: changed PDeviceContext into class TDeviceContext

  Revision 1.224  2002/09/30 20:37:09  lazarus
  MG: fixed transient of modal forms

  Revision 1.223  2002/09/30 20:19:12  lazarus
  MG: fixed flickering of modal forms

  Revision 1.222  2002/09/29 15:08:39  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>
    Patch includes:
      -fixes Problems with hiding modal forms
      -temporarily fixes TCustomForm.BorderStyle in bsNone
      -temporarily fixes problems with improper tabbing in TSynEdit

  Revision 1.221  2002/09/27 20:52:24  lazarus
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

  Revision 1.220  2002/09/20 13:11:12  lazarus
  MG: fixed TPanel and Frame3D

  Revision 1.219  2002/09/19 19:56:14  lazarus
  MG: accelerated designer drawings

  Revision 1.218  2002/09/18 17:07:28  lazarus
  MG: added patch from Andrew

  Revision 1.217  2002/09/16 17:34:37  lazarus
  MG: fixed mem leak in TComboBox

  Revision 1.216  2002/09/16 16:06:21  lazarus
  MG: replaced halt with raiseexception

  Revision 1.215  2002/09/16 15:56:01  lazarus
  Resize cursors in designer.

  Revision 1.214  2002/09/16 15:42:17  lazarus
  MG: fixed calling DestroyHandle if not HandleAllocated

  Revision 1.213  2002/09/13 16:58:27  lazarus
  MG: removed the 1x1 bitmap from TBitBtn

  Revision 1.212  2002/09/13 11:49:47  lazarus
  Cleanups, extended TStatusBar, graphic control cleanups.

  Revision 1.211  2002/09/12 05:56:16  lazarus
  MG: gradient fill, minor issues from Andrew

  Revision 1.210  2002/09/10 17:30:16  lazarus
  MG: added TLabel.WordWrap for gtk-interface from Vincent

  Revision 1.209  2002/09/10 10:00:27  lazarus
  MG: TListView now works handleless and SetSelection implemented

  Revision 1.208  2002/09/10 06:49:20  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.207  2002/09/08 19:09:55  lazarus
  Fixed and simplified TRadioButton

  Revision 1.206  2002/09/08 10:02:00  lazarus
  MG: fixed streaming visible=false

  Revision 1.205  2002/09/07 20:30:50  lazarus
  Make TComboboxes sort again, including in OI

  Revision 1.204  2002/09/07 12:14:51  lazarus
  EchoMode for TCustomEdit. emNone not implemented for GTK+, falls back to emPassword
  behaviour.

  Revision 1.203  2002/09/06 22:32:21  lazarus
  Enabled cursor property + property editor.

  Revision 1.202  2002/09/06 19:45:11  lazarus
  Cleanups plus a fix to TPanel parent/drawing problem.

  Revision 1.201  2002/09/06 15:57:35  lazarus
  MG: fixed notebook client area, send messages and minor bugs

  Revision 1.200  2002/09/05 13:46:19  lazarus
  MG: activated InvalidateControl for TWinControls

  Revision 1.199  2002/09/05 12:11:44  lazarus
  MG: TNotebook is now streamable

  Revision 1.198  2002/09/05 10:12:07  lazarus

  New dialog for multiline caption of TCustomLabel.
  Prettified TStrings property editor.
  Memo now has automatic scrollbars (not fully working), WordWrap and Scrollbars property
  Removed saving of old combo text (it broke things and is not needed). Cleanups.

  Revision 1.197  2002/09/04 12:57:31  lazarus

  Workaround GTK accelerator bug.

  Revision 1.196  2002/09/04 09:32:17  lazarus
  MG: improved streaming error handling

  Revision 1.195  2002/09/03 11:32:49  lazarus

  Added shortcut keys to labels
  Support for alphabetically sorting the properties
  Standardize message and add shortcuts ala Kylix
  Published BorderStyle, unpublished BorderWidth
  ShowAccelChar and FocusControl
  ShowAccelChar and FocusControl for TLabel, escaped ampersands now work.

  Revision 1.194  2002/09/03 08:07:20  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.193  2002/09/02 19:10:28  lazarus
  MG: TNoteBook now starts with no Page and TCustomPage has no auto names

  Revision 1.192  2002/09/01 16:11:22  lazarus
  MG: double, triple and quad clicks now work

  Revision 1.191  2002/08/31 11:37:10  lazarus
  MG: fixed destroying combobox

  Revision 1.190  2002/08/31 10:55:15  lazarus
  MG: fixed range check error in ampersands2underscore

  Revision 1.189  2002/08/31 07:58:21  lazarus
  MG: fixed resetting comobobox text

  Revision 1.188  2002/08/30 13:46:32  lazarus
  MG: added failure exit

  Revision 1.187  2002/08/30 12:32:22  lazarus
  MG: MoveWindowOrgEx, Splitted FWinControls/FControls, TControl drawing, Better DesignerDrawing, ...

  Revision 1.186  2002/08/30 10:06:07  lazarus
  Fixed alignment of multiline TLabel.
  Simplified and prettified MessageBoxen.

  Revision 1.185  2002/08/30 06:46:04  lazarus

  Use comboboxes. Use history. Prettify the dialog. Preselect text on show.
  Make the findreplace a dialog. Thus removing resiying code (handled by Anchors now anyway).
  Make Anchors work again and publish them for various controls.
  SelStart and Co. for TEdit, SelectAll procedure for TComboBox and TEdit.
  Clean up and fix some bugs for TComboBox, plus selection stuff.

  Revision 1.184  2002/08/29 00:07:02  lazarus
  MG: fixed TComboBox and InvalidateControl

  Revision 1.183  2002/08/28 09:40:49  lazarus
  MG: reduced paint messages and DC getting/releasing

  Revision 1.182  2002/08/27 18:45:13  lazarus
  MG: propedits text improvements from Andrew, uncapturing, improved comobobox

  Revision 1.181  2002/08/27 14:33:37  lazarus
  MG: fixed designer component deletion

  Revision 1.180  2002/08/27 06:40:50  lazarus
  MG: ShortCut support for buttons from Andrew

  Revision 1.179  2002/08/26 17:28:21  lazarus
  MG: fixed speedbutton in designmode

  Revision 1.178  2002/08/25 14:27:45  lazarus
  MG: fixed unallocated spinedit handle bug

  Revision 1.177  2002/08/24 12:55:00  lazarus
  MG: fixed mouse capturing, OI edit focus

  Revision 1.176  2002/08/24 08:07:15  lazarus
  MG: fixed double click recognition

  Revision 1.175  2002/08/24 07:09:04  lazarus
  MG: fixed bracket hilighting

  Revision 1.174  2002/08/24 06:51:22  lazarus
  MG: from Andrew: style list fixes, autosize for radio/checkbtns

  Revision 1.173  2002/08/22 16:43:35  lazarus
  MG: improved theme support from Andrew

  Revision 1.172  2002/08/22 07:30:15  lazarus
  MG: freeing more unused GCs

  Revision 1.171  2002/08/21 14:06:40  lazarus
  MG: added TDeviceContextMemManager

  Revision 1.170  2002/08/21 11:42:52  lazarus
  MG: reduced output

  Revision 1.169  2002/08/21 11:42:09  lazarus
  MG: fixed mem leaks

  Revision 1.168  2002/08/21 11:29:35  lazarus
  MG: fixed mem some leaks in ide and gtk

  Revision 1.167  2002/08/21 08:13:37  lazarus
  MG: accelerated new/dispose of gdiobjects

  Revision 1.166  2002/08/19 18:00:02  lazarus
  MG: design signals for gtk internal widgets

  Revision 1.165  2002/08/17 15:45:34  lazarus
  MG: removed ClientRectBugfix defines

  Revision 1.164  2002/08/16 17:47:38  lazarus
  MG: added some IDE menuicons, fixed submenu indicator bug

  Revision 1.163  2002/08/15 15:46:49  lazarus
  MG: added changes from Andrew (Clipping)

  Revision 1.162  2002/08/15 13:37:57  lazarus
  MG: started menuitem icon, checked, radio and groupindex

  Revision 1.161  2002/08/12 15:32:29  lazarus
  MG: started enhanced menuitem

  Revision 1.160  2002/08/09 18:04:18  lazarus
  MG: activated App_Paintable for TCustomForms

  Revision 1.159  2002/08/08 18:05:46  lazarus
  MG: added graphics extensions from Andrew Johnson

  Revision 1.158  2002/08/08 17:26:38  lazarus
  MG: added property TMenuItems.RightJustify

  Revision 1.157  2002/08/08 10:33:50  lazarus
  MG: main bar speedbar open arrow now shows recent projects and files

  Revision 1.156  2002/08/08 09:38:36  lazarus
  MG: recent file menus are now updated instantly

  Revision 1.155  2002/08/08 09:07:07  lazarus
  MG: TMenuItem can now be created/destroyed/moved at any time

  Revision 1.154  2002/08/05 10:45:04  lazarus
  MG: TMenuItem.Caption can now be set after creation

  Revision 1.153  2002/08/05 08:56:56  lazarus
  MG: TMenuItems can now be enabled and disabled

  Revision 1.152  2002/08/05 07:43:29  lazarus
  MG: fixed BadCursor bug and Circle Reference of FixedWidget of csPanel

  Revision 1.151  2002/08/04 07:09:28  lazarus
  MG: fixed client events

  Revision 1.150  2002/07/29 13:26:57  lazarus
  MG: source notebook pagenames are now updated more often

  Revision 1.149  2002/07/23 07:40:51  lazarus
  MG: fixed get widget position for inherited gdkwindows

  Revision 1.148  2002/07/20 13:47:03  lazarus
  MG: fixed eventmask for realized windows

  Revision 1.147  2002/07/09 17:46:58  lazarus
  MG: fixed setcolor

  Revision 1.146  2002/07/09 17:18:22  lazarus
  MG: fixed parser for external vars

  Revision 1.145  2002/06/26 15:11:09  lazarus
  MG: added new tool: Guess misplaced $IFDEF/$ENDIF

  Revision 1.144  2002/06/21 17:54:23  lazarus
  MG: in design mode the mouse cursor is now also set for hidden gdkwindows

  Revision 1.143  2002/06/21 16:59:15  lazarus
  MG: TControl.Cursor is now set, reduced auto reaction of widgets in design mode

  Revision 1.142  2002/06/19 19:46:09  lazarus
  MG: Form Editing: snapping, guidelines, modified on move/resize, creating components in csDesigning, ...

  Revision 1.141  2002/06/11 13:41:10  lazarus
  MG: fixed mouse coords and fixed mouse clicked thru bug

  Revision 1.140  2002/06/09 14:00:41  lazarus
  MG: fixed persistent caret and implemented Form.BorderStyle=bsNone

  Revision 1.139  2002/06/09 07:08:43  lazarus
  MG: fixed window jumping

  Revision 1.138  2002/06/08 17:16:04  lazarus
  MG: added close buttons and images to TNoteBook and close buttons to source editor

  Revision 1.137  2002/06/07 07:40:45  lazarus
  MG: goto bookmark now centers the cursor line

  Revision 1.136  2002/06/07 06:40:18  lazarus
  MG: gtk HandleEvents will now process all pending events

  Revision 1.135  2002/06/06 07:23:24  lazarus
  MG: small fixes to reduce form repositioing

  Revision 1.134  2002/06/05 19:04:15  lazarus
  MG: fixed LM_SetItemIndex gtk warning

  Revision 1.133  2002/06/05 12:33:57  lazarus
  MG: fixed fonts in XLFD format and styles

  Revision 1.132  2002/06/04 15:17:23  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.131  2002/05/31 06:45:22  lazarus
  MG: deactivated new system colors, till we got a consistent solution

  Revision 1.130  2002/05/30 14:11:12  lazarus
  MG: added filters and history to TOpenDialog

  Revision 1.129  2002/05/29 21:44:38  lazarus
  MG: improved TCommon/File/OpenDialog, fixed TListView scrolling and broder

  Revision 1.128  2002/05/28 19:39:45  lazarus
  MG: added gtk rc file support and started stule dependent syscolors

  Revision 1.127  2002/05/28 14:58:31  lazarus
  MG: added scrollbars for TListView

  Revision 1.126  2002/05/24 07:16:32  lazarus
  MG: started mouse bugfix and completed Makefile.fpc

  Revision 1.125  2002/05/16 15:42:54  lazarus
  MG: fixed TCustomForm ShowHide repositioning

  Revision 1.124  2002/05/15 05:58:17  lazarus
  MG: added TMainMenu.Parent

  Revision 1.123  2002/05/13 15:26:14  lazarus
  MG: fixed form positioning when show, hide, show

  Revision 1.122  2002/05/13 14:47:01  lazarus
  MG: fixed client rectangles, TRadioGroup, RecreateWnd

  Revision 1.121  2002/05/12 04:56:20  lazarus
  MG: client rect bugs nearly completed

  Revision 1.120  2002/05/10 06:05:57  lazarus
  MG: changed license to LGPL

  Revision 1.119  2002/05/09 12:41:29  lazarus
  MG: further clientrect bugfixes

  Revision 1.118  2002/05/06 08:50:36  lazarus
  MG: replaced logo, increased version to 0.8.3a and some clientrectbugfix

  Revision 1.117  2002/05/01 11:57:41  lazarus
  MG: find declaration for delphi pointer shortcut and clientrect tricks

  Revision 1.116  2002/04/30 09:57:21  lazarus
  MG: fixed find declaration of default properties

  Revision 1.115  2002/04/27 15:35:51  lazarus
  MG: fixed window shrinking

  Revision 1.114  2002/04/26 12:26:50  lazarus
  MG: improved clean up

  Revision 1.113  2002/03/29 19:11:38  lazarus
  Added Triple Click
  Shane

  Revision 1.112  2002/03/27 00:33:54  lazarus
  MWE:
    * Cleanup in lmessages
    * Added Listview selection and notification events
    + introduced commctrl

  Revision 1.111  2002/03/25 17:59:20  lazarus
  GTK Cleanup
  Shane

  Revision 1.110  2002/03/15 13:15:23  lazarus
  Removed FOCUSIN messages
  Removed Bitbtn created message
  Shane

  Revision 1.109  2002/03/14 20:28:49  lazarus
  Bug fix for Mattias.
  Fixed spinedit so you can now get the value and set the value.
  Shane

  Revision 1.108  2002/03/13 22:48:16  lazarus
  Constraints implementation (first cut) and sizig - moving system rework to
  better match Delphi/Kylix way of doing things (the existing implementation
  worked by acident IMHO :-)

  Revision 1.107  2002/03/12 23:55:37  lazarus
  MWE:
    * More delphi compatibility added/updated to TListView
    * Introduced TDebugger.locals
    * Moved breakpoints dialog to debugger dir
    * Changed breakpoints dialog to read from resource

  Revision 1.106  2002/03/11 23:07:23  lazarus
  MWE:
    * Made TListview more Delphi compatible
    * Did some cleanup

  Revision 1.105  2002/02/20 19:11:48  lazarus
  Minor tweaks, default font caching.

  Revision 1.104  2002/02/18 22:46:11  lazarus
  Implented TMenuItem.ShortCut (not much tested).

  Revision 1.103  2002/02/03 00:24:01  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.102  2002/01/24 15:40:59  lazarus
  MG: deactivated clipboard setting target list for win32

  Revision 1.101  2002/01/08 16:02:45  lazarus
  Minor changes to TListView.
  Added TImageList to the IDE
  Shane

  Revision 1.100  2002/01/04 20:29:04  lazarus
  Added images to TListView.
  Shane

  Revision 1.99  2002/01/03 21:17:08  lazarus
  added column visible and autosize settings.
  Shane

  Revision 1.98  2002/01/03 15:31:27  lazarus
  Added changes to propedit so the colum editor changes effect the TListView.
  Shane

  Revision 1.97  2002/01/01 15:50:16  lazarus
  MG: fixed initial component aligning

  Revision 1.96  2001/12/28 15:12:02  lazarus
  MG: LM_SIZE and LM_MOVE messages are now send directly, not queued

  Revision 1.95  2001/12/21 18:17:00  lazarus
  Added TImage class
  Shane

  Revision 1.94  2001/12/20 19:11:23  lazarus
  Changed the delay for the hints from 100 miliseconds to 500.  I'm hoping this reduces the crashing for some people until I determine the problem.
  Shane

  Revision 1.93  2001/12/19 21:36:05  lazarus
  Added MultiSelect to TListView
  Shane

  Revision 1.92  2001/12/19 20:28:51  lazarus
  Enabled Alignment of columns in a TListView.
  Shane

  Revision 1.91  2001/12/18 21:10:01  lazarus
  MOre additions for breakpoints dialog
  Added a TSynEditPlugin in SourceEditor to get notified of lines inserted and deleted from the source.
  Shane

  Revision 1.90  2001/12/16 22:24:55  lazarus
  MG: changes for new compiler 20011216

  Revision 1.89  2001/12/14 19:51:48  lazarus
  More changes to TListView
  Shane

  Revision 1.88  2001/12/14 18:38:56  lazarus
  Changed code for TListView
  Added a generic Breakpoints dialog
  Shane

  Revision 1.87  2001/12/12 20:19:19  lazarus

    Modified the the GTKFileSelection so that it will handle and use
    CTRL and SHIFT keys in a fashion similar to Windows.

  Revision 1.86  2001/12/12 14:39:25  lazarus
  MG: carets will now be auto destroyed on widget destroy

  Revision 1.85  2001/12/12 08:29:21  lazarus

        Add code to allow TOpenDialog to do multiple line selects.   MAH

  Revision 1.84  2001/12/11 16:51:37  lazarus
  Modified the Watches dialog
  Shane

  Revision 1.83  2001/12/11 14:36:41  lazarus
  MG: started multiselection for TOpenDialog

  Revision 1.82  2001/12/07 20:12:15  lazarus
  Added a watch dialog.
  Shane

  Revision 1.81  2001/12/06 13:39:36  lazarus
  Added TArrow component
  Shane

  Revision 1.80  2001/12/05 18:23:48  lazarus
  Added events to Calendar
  Shane

  Revision 1.79  2001/12/05 17:40:00  lazarus
  Added READONLY to Calendar.
  Shane

  Revision 1.77  2001/11/26 14:19:34  lazarus
  Added some code to make the interbae components work better.
  Shane

  Revision 1.75  2001/11/21 14:55:33  lazarus
  Changes for combobox to receive butondown and up events
  DblClick events now working.
  Shane

  Revision 1.74  2001/11/20 18:30:32  lazarus
  Pressing DEL when form is the only thing selected in designer no longer crashes Lazarus.
  Shane

  Revision 1.73  2001/11/17 09:42:26  lazarus
  MG: fixed range check errors for FG,BG in Init

  Revision 1.72  2001/11/16 20:08:39  lazarus
  Object inspector has hints now.
  Shane

  Revision 1.71  2001/11/14 17:46:58  lazarus
  Changes to make toggling between form and unit work.
  Added BringWindowToTop
  Shane

  Revision 1.70  2001/11/12 16:56:08  lazarus
  MG: CLIPBOARD

  Revision 1.69  2001/11/10 10:48:02  lazarus
  MG: fixed set formicon on invisible forms

  Revision 1.68  2001/11/09 19:14:24  lazarus
  HintWindow changes
  Shane

  Revision 1.67  2001/11/09 14:33:41  lazarus
  MG: fixed GetItemIndex-Handle-NotAllocated-Crash bug

  Revision 1.66  2001/11/05 18:18:19  lazarus
  added popupmenu+arrows to notebooks, added target filename

  Revision 1.65  2001/11/01 21:30:35  lazarus
  Changes to Messagebox.
  Added line to CodeTools to prevent duplicate USES entries.

  Revision 1.64  2001/10/31 16:29:22  lazarus
  Fixed the gtk mousemove bug where the control gets the coord's based on it's parent instead of itself.
  Shane

  Revision 1.63  2001/10/16 20:01:28  lazarus
  MG: removed splashform fix, because of the unpredictable side effects

  Revision 1.62  2001/10/16 10:51:10  lazarus
  MG: added clicked event to TButton, MessageDialog reacts to return key

  Revision 1.60  2001/10/09 09:46:59  lazarus
  MG: added codetools, fixed synedit unindent, fixed MCatureHandle

  Revision 1.59  2001/10/08 12:57:07  lazarus
  MG: fixed GetPixel

  Revision 1.58  2001/10/08 08:05:08  lazarus
  MG: fixed TColorDialog set color

  Revision 1.57  2001/10/07 07:28:34  lazarus
  MG: fixed setpixel and TCustomForm.OnResize event

  Revision 1.56  2001/09/30 08:34:52  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.55  2001/08/07 11:05:51  lazarus
  MG: small bugfixes

  Revision 1.54  2001/07/01 23:33:13  lazarus
  MG: added WaitMessage and HandleEvents is now non blocking

  Revision 1.53  2001/06/28 18:15:04  lazarus
  MG: bugfixes for destroying controls

  Revision 1.52  2001/06/26 21:44:32  lazarus
  MG: reduced paint messages

  Revision 1.51  2001/06/26 00:08:36  lazarus
  MG: added code for form icons from Rene E. Beszon

  Revision 1.49  2001/06/14 14:57:59  lazarus
  MG: small bugfixes and less notes

  Revision 1.47  2001/06/05 10:32:05  lazarus
  MG: small bugfixes for bitbtn, handles

  Revision 1.45  2001/05/13 22:07:09  lazarus
  Implemented BringToFront / SendToBack.

  Revision 1.44  2001/04/13 17:56:17  lazarus
  MWE:
  * Moved menubar outside clientarea
  * Played a bit with the IDE layout
  * Moved the creation of the toolbarspeedbuttons to a separate function

  Revision 1.43  2001/04/06 22:25:14  lazarus
  * TTimer uses winapi-interface now instead of sendmessage-interface, stoppok

  Revision 1.42  2001/03/27 21:12:54  lazarus
  MWE:
    + Turned on longstrings
    + modified memotest to add lines

  Revision 1.41  2001/03/27 14:27:43  lazarus
  Changes from Nagy Zsolt
  Shane

  Revision 1.40  2001/03/26 14:58:31  lazarus
  MG: setwindowpos + bugfixes

  Revision 1.36  2001/03/19 18:51:57  lazarus
  MG: added dynhasharray and renamed tsynautocompletion

  Revision 1.35  2001/03/19 14:44:22  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.31  2001/03/12 12:17:02  lazarus
  MG: fixed random function results

  Revision 1.30  2001/02/20 16:53:27  lazarus
  Changes for wordcompletion and many other things from Mattias.
  Shane

  Revision 1.29  2001/02/06 18:19:38  lazarus
  Shane

  Revision 1.28  2001/02/06 14:52:47  lazarus
  Changed TSpeedbutton in gtkobject so it erases itself when it's set to visible=false;
  Shane

  Revision 1.27  2001/02/04 04:18:12  lazarus
  Code cleanup and JITFOrms bug fix.
  Shane

  Revision 1.26  2001/02/02 20:13:39  lazarus
  Codecompletion changes.
  Added code to Uniteditor for code completion.

  Also, added code to gtkobject.inc so forms now get keypress events.
  Shane

  Revision 1.25  2001/02/01 19:34:50  lazarus
  TScrollbar created and a lot of code added.

  It's cose to working.
  Shane

  Revision 1.24  2001/01/31 21:16:45  lazarus
  Changed to TCOmboBox focusing.
  Shane

  Revision 1.23  2001/01/28 21:06:07  lazarus
  Changes for TComboBox events KeyPress Focus.
  Shane

  Revision 1.22  2001/01/28 03:51:42  lazarus
  Fixed the problem with Changed for ComboBoxs
  Shane

  Revision 1.21  2001/01/24 23:26:40  lazarus
  MWE:
    = moved some types to gtkdef
    + added WinWidgetInfo
    + added some initialization to Application.Create

  Revision 1.20  2001/01/24 03:21:03  lazarus
  Removed gtkDrawDefualt signal function from gtkcallback.inc
  It was no longer used.
  Shane

  Revision 1.19  2001/01/23 23:33:55  lazarus
  MWE:
    - Removed old LM_InvalidateRect
    - did some cleanup in old  code
    + added some comments  on gtkobject data (gtkproc)

  Revision 1.18  2001/01/13 03:09:37  lazarus
  Minor changes
  Shane

  Revision 1.17  2001/01/10 20:12:29  lazarus
  Added the Nudge feature to the IDE.
  Shane

  Revision 1.16  2001/01/09 18:23:21  lazarus
  Worked on moving controls.  It's just not working with the X and Y coord's I'm getting.
  Shane

  Revision 1.15  2001/01/04 15:09:05  lazarus
  Tested TCustomEdit.Readonly, MaxLength and CharCase.
  Shane

  Revision 1.14  2001/01/04 13:52:00  lazarus
  Minor changes to TEdit.
  Not tested.
  Shane

  Revision 1.13  2000/12/29 19:20:27  lazarus
  Shane

  Revision 1.11  2000/12/22 19:55:38  lazarus
  Added the Popupmenu code to the LCL.
  Now you can right click on the editor and a PopupMenu appears.
  Shane

  Revision 1.10  2000/12/19 18:43:13  lazarus
  Removed IDEEDITOR.  This causes the PROJECT class to not function.
  Saving projects no longer works.

  I added TSourceNotebook and TSourceEditor.  They do all the work for saving/closing/opening units.  Somethings work but they are in early development.
  Shane

  Revision 1.9  2000/10/09 22:50:32  lazarus
  MWE:
    * fixed some selection code
    + Added selection sample

  Revision 1.8  2000/09/10 23:08:31  lazarus
  MWE:
    + Added CreateCompatibeleBitamp function
    + Updated TWinControl.WMPaint
    + Added some checks to avoid gtk/gdk errors
    - Removed no fixed warning from GetDC
    - Removed some output

  Revision 1.7  2000/08/10 10:55:45  lazarus
  Changed TCustomDialog to TCommonDialog
  Shane

  Revision 1.6  2000/08/09 18:32:10  lazarus
  Added more code for the find function.
  Shane

  Revision 1.5  2000/07/30 21:48:33  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.4  2000/07/23 18:59:35  lazarus
  more cleanups, stoppok

  Revision 1.3  2000/07/23 10:51:53  lazarus
  - cleanups in IntSendMessage3
  - minor cleanups in other functions
  stoppok

  Revision 1.2  2000/07/16 20:59:03  lazarus
  - some more cleanups (removal of unused variables), stoppok

  Revision 1.1  2000/07/13 10:28:29  michael
  + Initial import

}
