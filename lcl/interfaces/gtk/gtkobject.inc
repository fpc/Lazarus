{******************************************************************************
                                   TGTKObject
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
  Method: TGtkObject.Create
  Params:  None
  Returns: Nothing

  Contructor for the class.
 ------------------------------------------------------------------------------}
constructor TGtkObject.Create; 
begin
  inherited Create;
  
  // DCs, GDIObjects
  FDeviceContexts := TDynHashArray.Create(-1);
  FDeviceContexts.Options:=FDeviceContexts.Options+[dhaoCacheContains];
  FGDIObjects := TDynHashArray.Create(-1);
  FGDIObjects.Options:=FGDIObjects.Options+[dhaoCacheContains];
  FDefaultFont:= nil;

  // messages
  FMessageQueue := TLazQueue.Create;
  FPaintMessages := TDynHashArray.Create(-1);
  FPaintMessages.OwnerHashFunction := @HashPaintMessage;
  WaitingForMessages := false;
  FWidgetsWithResizeRequest := TDynHashArray.Create(-1);
  FWidgetsWithResizeRequest.Options:=
    FWidgetsWithResizeRequest.Options+[dhaoCacheContains];
  FWidgetsResized := TDynHashArray.Create(-1);
  FWidgetsResized.Options:=FWidgetsResized.Options+[dhaoCacheContains];
  FFixWidgetsResized := TDynHashArray.Create(-1);
  
  FTimerData  := TList.Create;
  FKeyStateList := TList.Create;

  FRCFilename := ChangeFileExt(ParamStr(0),'.gtkrc');
  FRCFileParsed := false;

  // initialize app level gtk engine
  gtk_set_locale ();

  // call init and pass cmd line args
  PassCmdLineOptions;

  // read gtk rc file
  ParseRCFile;

  // Initialize Stringlist for holding styles
  Styles := TStringlist.Create;

  gtk_key_snooper_install(@GTKKeySnooper, @FKeyStateList);

  // Init tooltips
  FGTKToolTips := gtk_tooltips_new;
  //gtk_object_ref(PGTKObject(FGTKToolTips));
  gtk_toolTips_Enable(FGTKToolTips);

  // Init stock objects;
  InitStockItems;

  // clipboard
  ClipboardTypeAtoms[ctPrimarySelection]:=GDK_SELECTION_PRIMARY;
  ClipboardTypeAtoms[ctSecondarySelection]:=GDK_SELECTION_SECONDARY;
  ClipboardTypeAtoms[ctClipboard]:=gdk_atom_intern('CLIPBOARD',0);
end;

{------------------------------------------------------------------------------
  Method: Tgtkobject.PassCmdLineOptions
  Params:  None
  Returns: Nothing

  Passes command line options to the gtk engine
 ------------------------------------------------------------------------------}
procedure Tgtkobject.PassCmdLineOptions;
begin
  gtk_init (@argc, @argv);
end;

{------------------------------------------------------------------------------
  procedure TgtkObject.FreeAllStyles;
 ------------------------------------------------------------------------------}
procedure TgtkObject.FreeAllStyles;
var
  I : Integer;
begin
  If Assigned(Styles) then
    Try
      For I := Styles.Count - 1 downto 0 do
        ReleaseStyle(Styles[I]);
      Styles.Free;
    Except
    End;
  Styles := Nil;
end;

{------------------------------------------------------------------------------
  Method: Tgtkobject.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TgtkObject.Destroy;
const
  GDITYPENAME: array[TGDIType] of String = ('gdiBitmap', 'gdiBrush'
    ,'gdiFont', 'gdiPen', 'gdiRegion', 'gdiPalette');
var
  n: Integer;
  p: PMsg;
  pTimerInfo  : PGtkITimerinfo;
  GDITypeCount: array[TGDIType] of Integer;
  GDIType: TGDIType;
  HashItem: PDynHashArrayItem;
  QueueItem, OldQueueItem: PLazQueueItem;
begin
  FreeAllStyles;
  FreeGDKCursors;
  FreeStockItems;

  if FGTKToolTips<>nil then begin
    gtk_object_unref(PGTKObject(FGTKToolTips));
    FGTKToolTips := nil;
  end;

  // tidy up the messages
  QueueItem:=FMessageQueue.First;
  while (QueueItem<>nil) do begin
    p := PMsg(QueueItem^.Data);
    if (p^.Message=LM_PAINT) or (p^.Message=LM_GtkPAINT) then begin
      //writeln('[TgtkObject.Destroy]  freeing unused paint message ',HexStr(p^.WParam,8));
      FPaintMessages.Remove(QueueItem);
      if p^.Message=LM_PAINT then
        ReleaseDC(0,P^.WParam);
      Dispose(P);
      OldQueueItem:=QueueItem;
      QueueItem:=QueueItem^.Next;
      FMessageQueue.Delete(OldQueueItem);
    end else
      QueueItem:=QueueItem^.Next;
  end;
  
  if FPaintMessages.Count>0 then begin
    WriteLn('[TgtkObject.Destroy] WARNING: There are ',FPaintMessages.Count
       ,' unremoved LM_PAINT/LM_GtkPAINT message links left.');
  end;

  if (FDeviceContexts.Count > 0)
  then begin
    WriteLN(Format('[TgtkObject.Destroy] WARNING: There are %d unreleased DCs, a detailed dump follows:' ,[FDeviceContexts.Count]));
    
    n:=0;
    write('[TgtkObject.Destroy]   DCs:  ');
    HashItem:=FDeviceContexts.FirstHashItem;
    while (n<7) and (HashItem<>nil) do 
    begin
      write(' ',HexStr(Cardinal(HashItem^.Item),8));
      HashItem:=HashItem^.Next;
      inc(n);
    end;
    writeln();
  end;

  if (FGDIObjects.Count > 0)  
  then begin  
    WriteLN(Format('[TgtkObject.Destroy] WARNING: There are %d unreleased GDIObjects, a detailed dump follows:' ,[FGDIObjects.Count]));  
    for GDIType := Low(GDIType) to High(GDIType) do
      GDITypeCount[GDIType] := 0;
      
    n:=0;
    write('[TgtkObject.Destroy]   GDIOs:');
    HashItem := FGDIObjects.FirstHashItem;
    while (HashItem <> nil) do
    begin
      if n < 7
      then write(' ',HexStr(Cardinal(HashItem^.Item),8));

      Inc(GDITypeCount[PGdiObject(HashItem^.Item)^.GDIType]);
      HashItem := HashItem^.Next;
      Inc(n);
    end;
    Writeln();
    
    for GDIType := Low(GDIType) to High(GDIType) do 
      if GDITypeCount[GDIType] > 0 then
        WriteLN(Format('[TgtkObject.Destroy]   %s: %d', [GDITYPENAME[GDIType], GDITypeCount[GDIType]]));
  end;

  if FMessageQueue.Count > 0 
  then begin
    WriteLN(Format('[TgtkObject.Destroy] WARNING: There are %d messages left in the queue! I''ll free them' ,[FMessageQueue.Count]));
    while FMessageQueue.First<>nil do begin
      p := PMsg(FMessageQueue.First^.Data);
      Dispose(P);
      FMessageQueue.Delete(FMessageQueue.First);
    end;
  end;

  n := FTimerData.Count;
  if (n > 0) then
  begin
    WriteLN(Format('[TgtkObject.Destroy] WARNING: There are %d TimerInfo structures left, I''ll free them' ,[n]));
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
  FPaintMessages.Free;
  FDeviceContexts.Free;
  FGDIObjects.Free;
  FKeyStateList.Free;
  FTimerData.Free;
  
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.SetWindowSizeAndPosition
  Params:  Widget: PGtkWidget; AWinControl: TWinControl
  Returns: Nothing

  Set the size and position of a top level window.
 ------------------------------------------------------------------------------}
procedure TgtkObject.SetWindowSizeAndPosition(Window: PGtkWindow;
  AWinControl: TWinControl);
var
  Width, Height: integer;
  //Info: PGtkWindowGeometryInfo;
begin
  Width:=AWinControl.Width;
  if Width<0 then
    Width:=0;
  Height:=AWinControl.Height;
  if Height<0 then
    Height:=0;
    
  //writeln('TgtkObject.SetWindowSizeAndPosition ',AWinControl.Name,':',AWinControl.ClassName,' ',AWinControl.Visible,' Old=',PGtkWidget(Window)^.allocation.Width,',',PGtkWidget(Window)^.allocation.Width,' New=',Width,',',Height);
  // set geometry default size
  //Info:=gtk_window_get_geometry_info(Window, TRUE);
  //if (Info^.default_width<>Width) or (Info^.default_height<>Height) then
    gtk_window_set_default_size(Window,Width,Height);
    
  // resize
  //if (PGtkWidget(Window)^.allocation.Width<>Width)
  //and (PGtkWidget(Window)^.allocation.Height<>Height) then begin
    gtk_widget_set_usize(PGtkWidget(Window), -1,-1);
    gtk_widget_set_usize(PGtkWidget(Window),Width,Height);
  //end;
  
  // reposition
  //writeln('TgtkObject.SetWindowSizeAndPosition B ',AWinControl.Name,':',AWinControl.ClassName,' ',AWinControl.Visible,' Old=',PGtkWidget(Window)^.allocation.X,',',PGtkWidget(Window)^.allocation.Y,' New=',AWinControl.Left,',',AWinControl.Top);
  gtk_widget_set_uposition(PGtkWidget(Window),AWinControl.Left,AWinControl.Top);
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.SendCachedLCLMessages
  Params:  None
  Returns: Nothing

  Some LCL messages are not sent directly to the gtk. Send them now.
 ------------------------------------------------------------------------------}
procedure TgtkObject.SendCachedLCLMessages;

  procedure SendCachedLCLResizeRequests;
  var
    Widget, ParentFixed, ParentWidget: PGtkWidget;
    LCLControl: TControl;
    IsTopLevelWidget: boolean;
    TopologicalList: TList; // list of PGtkWidget;
    i, LCLWidth, LCLHeight: integer;
  begin
    if FWidgetsWithResizeRequest.Count=0 then exit;
    {$IFDEF VerboseSizeMsg}
    writeln('GGG1 SendCachedLCLResizeRequests SizeMsgCount=',FWidgetsWithResizeRequest.Count);
    {$ENDIF}
    
    TopologicalList:=CreateTopologicalSortedWidgets(FWidgetsWithResizeRequest);
    for i:=0 to TopologicalList.Count-1 do begin
      Widget:=TopologicalList[i];

      // resize widget
      LCLControl:=TControl(GetLCLObject(Widget));
      if (LCLControl=nil) or (not (LCLControl is TControl)) then begin
        RaiseException('ERROR: TgtkObject.SendCachedLCLMessages Widget '
          +HexStr(Cardinal(Widget),8)+' without LCL control');
      end;
      {$IFDEF VerboseSizeMsg}
      writeln('SendCachedLCLMessages ',LCLControl.Name,':',LCLControl.ClassName,
        ' ',LCLControl.Left,',',LCLControl.Top,',',LCLControl.Width,',',LCLControl.Height);
      {$ENDIF}
      
      IsTopLevelWidget:= (LCLControl is TCustomForm)
                         and (LCLControl.Parent = nil);

      if not IsTopLevelWidget then begin
        // resize widget
        LCLWidth:=LCLControl.Width;
        if LCLWidth<0 then
          LCLWidth:=0;
        LCLHeight:=LCLControl.Height;
        if LCLHeight<0 then
          LCLHeight:=0;
        if (LCLWidth>10000) or (LCLHeight>10000) then begin
          writeln('WARNING: resizing BIG ',
            ' Control=',LCLControl.Name,':',LCLControl.ClassName,
            ' NewSize=',LCLWidth,',',LCLHeight);
        end;
        gtk_widget_set_usize(Widget, LCLWidth, LCLHeight);
        // move widget on the fixed widget of parent control
        ParentWidget:=pgtkWidget(LCLControl.Parent.Handle);
        ParentFixed := GetFixedWidget(ParentWidget);
        if GtkWidgetIsA(ParentFixed,GTK_FIXED_GET_TYPE)
        or GtkWidgetIsA(ParentFixed,GTK_LAYOUT_GET_TYPE) then begin
          FixedMoveControl(ParentFixed, Widget,
                         LCLControl.Left, LCLControl.Top);
        end else if not (LCLControl.Parent is TNoteBook) then begin
          writeln('WARNING: TgtkObject.SendCachedLCLMessages - Parent''s Fixed Widget not found');
          writeln('  Control=',LCLControl.Name,':',LCLControl.ClassName,
            ' Parent=',LCLControl.Parent.Name,':',LCLControl.Parent.ClassName,
            ' ParentWidget=',HexStr(Cardinal(ParentWidget),8),
            '');
        end;
      end
      else begin
        // resize form
        {$IFDEF VerboseFormPositioning}
        writeln('VFP SendCachedLCLMessages1 ',GetControlWindow(Widget)<>nil);
        if (LCLControl is TCustomForm) then
          writeln('VFP SendCachedLCLMessages2 ',LCLControl.ClassName,' ',
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
  Method: TGtkObject.LCLtoGtkMessagePending
  Params:  None
  Returns: boolean

  Returns true if any messages from the lcl to the gtk is in cache and needs
  delivery.
 ------------------------------------------------------------------------------}
function TgtkObject.LCLtoGtkMessagePending: boolean;
begin
  Result:=(FWidgetsWithResizeRequest.Count>0);
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.SendCachedGtkMessages
  Params:  None
  Returns: Nothing

  Some Gtk messages are not sent directly to the LCL. Send them now.
 ------------------------------------------------------------------------------}
procedure TGtkObject.SendCachedGtkMessages;

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
    LCLControl:=TWinControl(GetLCLObject(MainWidget));
    if LCLControl=nil then exit;
    {$IFDEF VerboseSizeMsg}
    writeln('JJJ1 SendSizeNotificationToLCL ',LCLControl.Name,':',LCLControl.ClassName);
    {$ENDIF}

    GtkLeft:=MainWidget^.Allocation.X;
    GtkTop:=MainWidget^.Allocation.Y;
    GtkWidth:=MainWidget^.Allocation.Width;
    if GtkWidth<0 then GtkWidth:=0;
    GtkHeight:=MainWidget^.Allocation.Height;
    if GtkHeight<0 then GtkHeight:=0;
    
    IsTopLevelWidget:=(LCLControl is TCustomForm)
                      and (LCLControl.Parent=nil);
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
      writeln('VFP SendSizeNotificationToLCL ',LCLControl.ClassName,' ',
        GtkLeft,',',GtkTop,',',GtkWidth,',',GtkHeight);
      {$ENDIF}
    end;
    
    UpdateLCLRect;

    {$IFDEF VerboseSizeMsg}
    writeln('JJJ2 ',LCLControl.Name,
      ' GTK=',GtkLeft,',',GtkTop,',',GtkWidth,',',GtkHeight,
      ' LCL=',LCLLeft,',',LCLTop,',',LCLWidth,',',LCLHeight
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
      if not MessageDelivered then exit;
      UpdateLCLRect;
    end;

    // then send a LM_SIZE message
    if WidthHeightChanged then begin
      {$IFDEF VerboseSizeMsg}
      writeln('JJJ3 Send LM_SIZE To LCL ',LCLControl.Name,':',LCLControl.ClassName);
      {$ENDIF}
      with SizeMsg do
      begin
        Result := 0;
        Msg := LM_SIZE;
        SizeType := Size_SourceIsInterface;
        Width := GtkWidth;
        Height := GtkHeight;
      end;
      MessageDelivered := (DeliverMessage(LCLControl, SizeMsg) = 0);
      if not MessageDelivered then exit;
      UpdateLCLRect;
    end;

    // then send a LM_MOVE message
    if TopLeftChanged then begin
      {$IFDEF VerboseSizeMsg}
      writeln('JJJ4 Send LM_MOVE To LCL ',LCLControl.Name,':',LCLControl.ClassName);
      {$ENDIF}
      with MoveMsg do
      begin
        Result := 0;
        Msg := LM_MOVE;
        MoveType := Move_SourceIsInterface;
        XPos := GtkLeft;
        YPos := GtkTop;
      end;
      MessageDelivered := (DeliverMessage(LCLControl,  MoveMsg) = 0);
      if not MessageDelivered then exit;
    end;
  end;
  
  procedure SendCachedGtkResizeNotifications;
  { This proc sends all cached size messages from the gtk to lcl but in an
    optimized order. When sending the LCL a size/move/windowposchanged messages
    it will automatically realign all child controls. This realigning is based
    on the clientrect.
    Therefore, before a size message is sent to the lcl, all clientrect must
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
  begin
    if (FFixWidgetsResized.Count=0) and (FWidgetsResized.Count=0) then exit;
    
    List:=TList.Create;

    { if any fixed widget was resized then a client area of a LCL control was
      resized
      -> invalidate client rectangles
    }
    {$IFDEF VerboseSizeMsg}
    writeln('HHH1 SendCachedGtkClientResizeNotifications Invalidating ClientRects ... FixSizeMsgCount=',FFixWidgetsResized.Count);
    {$ENDIF}
    FFixWidgetsResized.AssignTo(List);
    for i:=0 to List.Count-1 do begin
      FixWidget:=List[i];
      MainWidget:=GetMainWidget(FixWidget);
      LCLControl:=TWinControl(GetLCLObject(MainWidget));
      if (LCLControl=nil) or (not (LCLControl is TWinControl)) then
        raise Exception.Create('SendCachedGtkResizeNotifications'
          +' FixWidget='+HexStr(Cardinal(FixWidget),8)
          +' MainWidget='+HexStr(Cardinal(MainWidget),8)
          +' LCLControl='+HexStr(Cardinal(LCLControl),8)
          );
      LCLControl.InvalidateClientRectCache;
    end;
    
    { if any main widget (= not fixed widget) was resized
      then a LCL control was resized
      -> send WMSize, WMMove, and WMWindowPosChanged messages
    }
    {$IFDEF VerboseSizeMsg}
    writeln('HHH2 SendCachedGtkClientResizeNotifications SizeMsgCount=',FWidgetsResized.Count);
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
    until false;

    { if any client area was resized, which MainWidget Size was already in sync
      with the LCL, no message was send. So, tell each changed client area to
      check its size.
    }
    {$IFDEF VerboseSizeMsg}
    writeln('HHH3 SendCachedGtkClientResizeNotifications Updating ClientRects ...');
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
    until false;

    List.Free;
    {$IFDEF VerboseSizeMsg}
    writeln('HHH4 SendCachedGtkClientResizeNotifications  completed.');
    {$ENDIF}
  end;

begin
  SendCachedGtkResizeNotifications;
end;

{------------------------------------------------------------------------------
  procedure TGtkObject.SendPaintMessagesForInternalWidgets(
    AWinControl: TWinControl);


 ------------------------------------------------------------------------------}
procedure TGtkObject.SendPaintMessagesForInternalWidgets(
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
  begin
    if PaintWindow=nil then exit;
    // check if PaintWindow is only used internally
    // and was not already used for an internal paint message
    if (PaintWindow=nil) or (PaintWindow=Context.MainWindow)
    or (PaintWindow=Context.ClientWindow)
    or ((Context.WindowList<>nil)
      and (Context.WindowList.IndexOf(PaintWindow)>=0))
    then exit;

    AMessage.Msg := LM_INTERNALPAINT;
    AMessage.WParam := CreateDCForWidget(Context.MainWidget,PaintWindow);
    AMessage.LParam := 0;
    AMessage.Result := 0;

    if Context.WindowList=nil then
      Context.WindowList:=TList.Create;
    Context.WindowList.Add(PaintWindow);

    {writeln('SendInternalPaintMessage ',
      AWinControl.Name,':',AWinControl.ClassName,
      ' InternalWindow=',HexStr(Cardinal(PaintWindow),8));}
    DeliverMessage(AWinControl,AMessage);

    if AMessage.WParam<>0 then
      ReleaseDC(0,HDC(AMessage.WParam));
  end;
  
  procedure ForAllChilds(PaintWidget: PgtkWidget);
  var
    LCLObject: TObject;
    ChildEntry: PGSList;
  begin
    if PaintWidget=nil then exit;
    LCLObject:=GetLCLObject(PaintWidget);
    if (LCLObject<>nil) and (LCLObject<>AWinControl) then exit;
    // send the paint message
    SendPaintMessageForGDkWindow(GetControlWindow(PaintWidget));

    // search all child widgets
    if GtkWidgetIsA(PaintWidget,GTK_CONTAINER_TYPE) then begin
      // this is a container widget -> go through all childs
      ChildEntry:=PGtkContainer(PaintWidget)^.resize_widgets;
      while ChildEntry<>nil do begin
        if PGtkWidget(ChildEntry^.Data)<>PaintWidget then
          ForAllChilds(PGtkWidget(ChildEntry^.Data));
        ChildEntry:=ChildEntry^.Next;
      end;
    end;
    if GtkWidgetIsA(PaintWidget,GTK_SCROLLED_WINDOW_TYPE) then begin
      ForAllChilds(PGtkScrolledWindow(PaintWidget)^.hscrollbar);
      ForAllChilds(PGtkScrolledWindow(PaintWidget)^.vscrollbar);
    end;
    if GtkWidgetIsA(PaintWidget,GTK_BIN_TYPE) then begin
      ForAllChilds(PGtkBin(PaintWidget)^.child);
    end;
    if GtkWidgetIsA(PaintWidget,GTK_COMBO_TYPE) then begin
      ForAllChilds(PGtkCombo(PaintWidget)^.entry);
      ForAllChilds(PGtkCombo(PaintWidget)^.button);
    end;
    if GtkWidgetIsA(PaintWidget,GTK_RANGE_TYPE) then begin
      SendPaintMessageForGDkWindow(PGtkRange(PaintWidget)^.slider);
      SendPaintMessageForGDkWindow(PGtkRange(PaintWidget)^.trough);
      SendPaintMessageForGDkWindow(PGtkRange(PaintWidget)^.step_forw);
      SendPaintMessageForGDkWindow(PGtkRange(PaintWidget)^.step_back);
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
  {writeln('TGtkObject.SendPaintMessagesForInternalWidgets START ',
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
  Method: TGtkObject.HandleEvents
  Params:  None
  Returns: Nothing

  Handle all pending messages of the GTK engine and of this interface
 ------------------------------------------------------------------------------}
procedure TgtkObject.HandleEvents;

  function PendingGtkMessagesExists: boolean;
  begin
    Result:=(gtk_events_pending<>0) or LCLtoGtkMessagePending;
  end;

var
  Msg: TMsg; 
  p: pMsg;
  IsPaintMessage: boolean;
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
    with FMessageQueue do begin
      while First<>nil do
      begin
        // fetch first message
        p := PMsg(First^.Data);
        Msg := p^;
        IsPaintMessage:=(Msg.Message=LM_PAINT) or (Msg.Message=LM_GtkPaint);

        // remove message from queue
        if IsPaintMessage then begin
          // paint messages are the most expensive messages in the LCL,
          // therefore they are sent always after all other
          if Count>FPaintMessages.Count then begin
            // there are non paint messages -> keep paint message back
            MoveToLast(First);
            continue;
          end else begin
            // there are only paint messages left in the queue
            // -> check other queues
            if PendingGtkMessagesExists then break;
          end;
          FPaintMessages.Remove(First);
        end;
        Delete(First);
        
        // Send message
        with Msg do
          SendMessage(hWND, Message, WParam, LParam);
        Dispose(p);
      end;
    end;
    
    // proceed until all messages are handled
  until not PendingGtkMessagesExists;
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.WaitMessage
  Params:  None
  Returns: Nothing

  Passes execution control to the GTK engine till something happens
 ------------------------------------------------------------------------------}
procedure TgtkObject.WaitMessage;
begin
  // process messages already in the queues
  HandleEvents;
  // wait till something happens
  WaitingForMessages:=true;
  gtk_main_iteration_do(True);
  WaitingForMessages:=false;
  // process new messages
  HandleEvents;
end;


procedure TGtkObject.FreeStockItems;

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
  Method: TGtkObject.AppTerminate
  Params:  None
  Returns: Nothing

  *Note: Tells GTK Engine to halt and destroy
 ------------------------------------------------------------------------------}
procedure TGtkObject.AppTerminate;
begin
  FreeAllStyles;
  // MG: using gtk_main_quit is not a clean way to close
  //gtk_main_quit;
end;

Procedure Tgtkobject.InitStockItems;
var
  LogBrush: TLogBrush;
  logPen : TLogPen;
begin
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
  Method: TGtkObject.AppInit
  Params:  None
  Returns: Nothing

  *Note: Initialize GTK engine
  (is called by TApplication.Initialize which is typically after all
   finalization sections)
 ------------------------------------------------------------------------------}
procedure TGtkObject.AppInit;
begin
  If Assigned(Screen) then
    FillScreenFonts(Screen.Fonts);
end;


{------------------------------------------------------------------------------
  Method: TGtkObject.RecreateWnd
  Params:  Sender: TObject - the lcl wincontrol, that is to recreated
  Returns: none
  
  Destroys Handle and child Handles and recreates them.
-------------------------------------------------------------------------------}
function TgtkObject.RecreateWnd(Sender: TObject): Integer;
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
function TGTKObject.CreateTimer(Interval: integer; TimerFunc: TFNTimerProc) : integer;
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
    writeln('TGTKObject.SetTimer ',HexStr(Cardinal(TimerInfo),8),' CurTimerCount=',FTimerData.Count,' OldTimerCount=',FOldTimerData.Count);
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
function TGTKObject.DestroyTimer(TimerHandle: integer) : boolean;
var
  n : integer;
  TimerInfo : PGtkITimerinfo;
begin
  Assert(False, 'Trace:removing timer!!!');
  n := FTimerData.Count;
  while (n > 0) do begin
    dec (n);
    TimerInfo := PGtkITimerinfo(FTimerData.Items[n]);
    if (TimerInfo^.TimerHandle=TimerHandle) then
    begin
      {$IFDEF VerboseTimer}
      writeln('TGTKObject.KillTimer TimerInfo=',HexStr(Cardinal(TimerInfo),8),' TimerHandle=',TimerHandle,' CurTimerCount=',FTimerData.Count,' OldTimerCount=',FOldTimerData.Count);
      {$ENDIF}
      gtk_timeout_remove(TimerInfo^.TimerHandle);
      FTimerData.Delete(n);
      Dispose(TimerInfo);
    end;
  end;
  Result:=true;
end;

procedure TGtkObject.LoadFromXPMFile(Bitmap: TObject; Filename: PChar);
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
    raise Exception.Create('TGtkObject.LoadFromXPMFile Bitmap is not TBitmap: '
                           +Bitmap.ClassName);
  TheBitmap:=TBitmap(Bitmap);
  GdiObject := NewGDIObject(gdiBitmap);
  if TheBitmap.TransparentColor<>clNone then begin
    GDKColor := AllocGDKColor(ColorToRGB(TheBitmap.TransparentColor));
    p := @GDKColor;
  end else
    p:=nil; // automatically create transparency mask
  Window:=nil; // use the X root window for colormap
  if Window<>nil then
    ColorMap:=gdk_window_get_colormap(Window)
  else
    ColorMap:=gdk_colormap_get_system;
  GdiObject^.GDIPixmapObject :=
    gdk_pixmap_colormap_create_from_xpm(Window,Colormap,
           @(GdiObject^.GDIBitmapMaskObject), p, Filename);
  GdiObject^.GDIBitmapType:=gbPixmap;
  gdk_window_get_geometry(GdiObject^.GDIPixmapObject,
    nil, nil, @Width, @Height, @Depth);

 If GdiObject^.Visual <> nil then
   GDK_Visual_UnRef(GdiObject^.Visual);

 If GdiObject^.Colormap <> nil then
   GDK_Colormap_UnRef(GdiObject^.Colormap);

  GdiObject^.Visual := gdk_window_get_visual(GdiObject^.GDIpixmapObject);
  If GdiObject^.Visual = nil then
    GdiObject^.Visual := gdk_visual_get_best_with_depth(Depth)
  else
    gdk_visual_ref(GdiObject^.Visual);

  GdiObject^.Colormap := gdk_colormap_new(GdiObject^.Visual, 1);

  TheBitmap.Handle := HBITMAP(GdiObject);
  If GdiObject^.GDIBitmapMaskObject <> nil then
    TheBitmap.Transparent := True
  else
    TheBitmap.Transparent := False;
end;

procedure TGtkObject.LoadFromPixbufFile(Bitmap: TObject; Filename: PChar);
var
  TheBitmap: TBitmap;

  function LoadFile : Boolean;
  {$Ifndef NoGdkPixbufLib}
  var
    Src : PGDKPixbuf;
    Tmp : hBitmap;
    Width, Height,
    Depth : Longint;
  begin
    Result := False;
    SRC := nil;
    SRC := gdk_pixbuf_new_from_file(FileName);
    If SRC = nil then
      exit;
    Width := gdk_pixbuf_get_width(Src);
    Height := gdk_pixbuf_get_height(Src);

    TMP := CreateCompatibleBitmap(0, Width, Height);
    gdk_pixbuf_render_pixmap_and_mask(Src,@PGDIObject(TMP)^.GDIPixmapObject,
      PPGDKBitmap(@PGDIObject(TMP)^.GDIBitmapMaskObject), 0);

    gdk_window_get_geometry(PGDIObject(TMP)^.GDIPixmapObject,
      nil, nil, nil, nil, @Depth);

    If PGDIObject(TMP)^.Visual <> nil then
      GDK_Visual_UnRef(PGDIObject(TMP)^.Visual);

    PGDIObject(TMP)^.Visual := gdk_window_get_visual(PGDIObject(TMP)^.GDIPixmapObject);
    If PGDIObject(TMP)^.Visual = nil then
      PGDIObject(TMP)^.Visual := gdk_visual_get_best_with_depth(Depth)
    else
      GDK_Visual_Ref(PGDIObject(TMP)^.Visual);

    If PGDIObject(TMP)^.Colormap <> nil then
      GDK_Colormap_UnRef(PGDIObject(TMP)^.Colormap);

    PGDIObject(TMP)^.Colormap := gdk_colormap_new(PGDIObject(TMP)^.Visual, 1);

    TheBitmap.Handle := TMP;
    GDK_Pixbuf_Unref(Src);
    Result := True;
  {$Else not NoGdkPixbufLib}
  begin
    WriteLn('WARNING: [TgtkObject.LoadFromPixbufFile] GDKPixbuf support has been disabled, unable to load files!');
    Result := True;
  {$EndIf}
  end;

begin
  if not (Bitmap is TBitmap) then
    raise Exception.Create('TGtkObject.LoadFromPixbufFile Bitmap is not TBitmap: '
                           +Bitmap.ClassName);
  TheBitmap:=TBitmap(Bitmap);
  if not LoadFile then
    Writeln('WARNING: [TgtkObject.LoadFromPixbufFile] loading file FAILED!');
end;

procedure TGtkObject.LoadFromPixbufData(Bitmap : hBitmap; Data : PByte);
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
    Depth : Longint;
  begin
    Result := False;

    FillBitmapInfo(Bitmap, BMPInfo);

    Loader := gdk_pixbuf_loader_new;
    If Loader = nil then
      exit;

    SRC := nil;

    If gdk_pixbuf_loader_write(Loader, PChar(@BMPInfo), SizeOf(BMPInfo) div
       SizeOf(Char))
    then begin
      If gdk_pixbuf_loader_write(Loader, Pchar(Data), BMPInfo.InfoHeader.biSizeImage) then begin
          SRC := gdk_pixbuf_loader_get_pixbuf(loader);
      end
      else
        WriteLn('WARNING: [TgtkObject.LoadFromPixbufData] Error occured loading Data!');
    end
    else
      WriteLn('WARNING: [TgtkObject.LoadFromPixbufData] Error occured loading Bitmap Header!');
    gdk_pixbuf_loader_close(Loader);

    If SRC = nil then
      exit;

    With PGDIObject(Bitmap)^ do begin
      gdk_pixbuf_render_pixmap_and_mask(Src,@GDIPixmapObject,
        PPGDKBitmap(@GDIBitmapMaskObject), 0);

      gdk_window_get_geometry(GDIPixmapObject,
        nil, nil, nil, nil, @Depth);

      If Visual <> nil then
        GDK_Visual_UnRef(Visual);

      Visual := gdk_window_get_visual(GDIPixmapObject);
      If Visual = nil then
        Visual := gdk_visual_get_best_with_depth(Depth)
      else
        GDK_Visual_Ref(Visual);

      If Colormap <> nil then
        GDK_Colormap_UnRef(Colormap);

      Colormap := gdk_colormap_new(Visual, 1);

      GDK_Pixbuf_Unref(Src);
    end;

    Result := True;
  {$Else not NoGdkPixbufLib}
  begin
    WriteLn('WARNING: [TgtkObject.LoadFromPixbufData] GDKPixbuf support has been disabled, unable to load data!');
    Result := True;
  {$EndIf}
  end;

begin
  if not LoadData then
    Writeln('WARNING: [TgtkObject.LoadFromPixbufData] loading data FAILED!');
end;

procedure TGtkObject.ListViewChangeItem(TheListView: TObject; Index: integer);
var
  ListView: TListView;
  LVWidget: PgtkCList;
  pStr: PChar;
  ListItem: TListItem;
  i, ColCount: integer;
  Pixmap: PGdkPixmap;
  Mask: PGdkBitmap;
  ImageBitmap, MaskBitmap: TBitmap;
begin
  ListView:=TListView(TheListView);
  LVWidget:= PgtkCList(
           GetWidgetInfo(Pointer(ListView.Handle), True)^.ImplementationWidget);
  ListItem := ListView.Items[Index];
  // set caption (= first column text)
  pStr:=PChar(ListItem.Caption);
  if pStr=nil then pStr:=#0;
  gtk_clist_set_text(LVWidget,Index,0,pStr);

  // set image
  if (ListView.SmallImages <> nil) and (ListItem.ImageIndex >= 0)
  and (ListItem.ImageIndex < Listview.SmallImages.Count)
  then begin
    //draw image
    ListView.SmallImages.GetInternalImage(ListItem.ImageIndex,
      ImageBitmap, MaskBitmap);
    Pixmap:=PgdiObject(ImageBitmap.Handle)^.GDIPixmapObject;
    Mask:=pgdkBitmap(PgdiObject(ImageBitmap.Handle)^.GDIBitmapMaskObject);
    {$IfNDef Win32}
    gtk_clist_set_pixtext(LVWidget,Index,0,pStr,3,Pixmap,Mask);
    {$EndIF}
  end;

  // set the other column texts
  ColCount:=LVWidget^.Columns;
  for i := 1 to ColCount-1 do
  begin
    if i<=ListItem.SubItems.Count then begin
      // the first subitem is the second column
      pStr:=PChar(ListItem.SubItems.Strings[i-1]);
      if pStr=nil then pStr:=#0;
    end else begin
      pStr:=#0
    end;
    gtk_clist_set_text(LVWidget,Index,i,pStr);
  end;
end;

{------------------------------------------------------------------------------
  procedure TgtkObject.ListViewAddItem(TheListView: TObject);
------------------------------------------------------------------------------}
procedure TgtkObject.ListViewAddItem(TheListView: TObject);
var
  ListView: TListView;
  ListViewWidget: PGtkCList;
  Titles: PPGChar;
  i, Count: integer;
begin
  ListView:=TListView(TheListView);
  ListViewWidget:= PGtkCList(GetWidgetInfo(
                         Pointer(ListView.Handle), True)^.ImplementationWidget);
  Count:=ListViewWidget^.columns;
  if Count=0 then begin
    writeln('WARNING: ListViewWidget^.columns=0');
    exit;
  end;
  GetMem(Titles,SizeOf(PGChar)*Count);
  Titles[0]:=#0;
  for i:=1 to Count-1 do Titles[i]:=nil;
  gtk_clist_append(ListViewWidget,Titles);
  FreeMem(Titles);
end;

{------------------------------------------------------------------------------
  procedure TgtkObject.BringFormToFront(Sender: TObject);
------------------------------------------------------------------------------}
procedure TgtkObject.BringFormToFront(Sender: TObject);
var
  AWindow: PGdkWindow;
  Widget: PGtkWidget;
begin
  Widget := PgtkWidget(TCustomForm(Sender).Handle);
  AWindow:=GetControlWindow(Widget);
  if AWindow<>nil then
    gdk_window_raise(AWindow);
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.IntSendMessage3
  Params:  LM_Message - message to be processed by GTK
           Sender     - sending control
           data       - pointer to (optional)
  Returns: depends on the message and the sender

  Processes messages from different components. 
  
  WARNING: the result of this function sometimes is not always really an
           integer!!!!!
 ------------------------------------------------------------------------------}
function TgtkObject.IntSendMessage3(LM_Message : Integer; Sender : TObject;
  data : pointer) : integer;
var
  handle    : hwnd;                  // handle of sender        
  pStr      : PChar;                 // temporary string pointer, must be allocated/disposed when used!
  Widget    : PGtkWidget;            // pointer to gtk-widget (local use when neccessary)
  AParent   : TWinControl;           // only used twice, replace with typecasts!
  Pixmap    : pgdkPixMap;
  box1      : pgtkWidget;            // currently only used for TBitBtn
  pixmapwid : pGtkWidget;            // currently only used for TBitBtn, possibly replace with pixmap!!!!
  pLabel    : PgtkWidget;            // currently only used as extra label-widget for TBitBtn
  Num       : Integer;               // currently only used for LM_INSERTTOOLBUTTON and LM_ADDITEM
  pStr2     : PChar;                 // currently only used for LM_INSERTTOOLBUTTON
  GList     : pGList;                // Only used for listboxes, replace with widget!!!!!
  SelectionMode : TGtkSelectionMode; // currently only used for listboxes
  ListItem  : PGtkListItem;          // currently only used for listboxes
  Rect      : TRect;
  FormIconGdiObject: PGdiObject;     // currently only used by LM_SETFORMICON
  Geometry  : TGdkGeometry;
  Accel     : integer;
  AWindow   : PGdkWindow;
begin
  Result := 0;   //default value just in case nothing sets it

  Assert(False, 'Trace:Message received'); 
  if Sender <> nil then
    Assert(False, Format('Trace:  [TgtkObject.IntSendMessage3] %s --> Sent LM_Message: $%x (%s); Data: %d', [Sender.ClassName, LM_Message, GetMessageName(LM_Message), Integer(data)]));

  // The following case is now split into 2 separate parts:
  //   1st part should contain all messages which don't need the "handle" variable
  //   2nd part has to contain all parts which need the handle
  // Reason for this split are performance issues since we need RTTI to
  // retrieve the handle
  case LM_Message of
    LM_Create   : CreateComponent(Sender);

    LM_SETCOLOR : SetColor(Sender);

    LM_SETPixel : SetPixel(Sender,Data); 

    LM_GETPixel : GetPixel(Sender,Data); 

    LM_ShowHide :
      begin
        //writeln('LM_ShowHide');
        ShowHide(Sender);
      end;

    LM_SetCursor : SetCursor(TWinControl(Sender), Data);

    LM_SetLabel  : SetLabel(Sender,Data);

    LM_GETVALUE  : Result := GetValue (Sender, data);

    LM_SETVALUE  : Result := SetValue (Sender, data);
   
    LM_SETPROPERTIES: Result := SetProperties(Sender);
    
    LM_SETDESIGNING :
      // Used by the form editor to set anything specifically needed
      // when setting controls to Designing.
      begin
        // change cursor
        if Sender is TWinControl then
          SetCursor(TWinControl(Sender), Data);
      end;

    LM_RECREATEWND : Result := RecreateWnd(sender);
    
    LM_ATTACHMENU: AttachMenu(Sender);
    
    LM_NB_UpdateTab: UpdateNotebookPageTab(nil,TPage(Sender));
    
  else begin
    handle := hwnd(ObjectToGtkObject(Sender));
    //???   if handle = nil then assert (false, Format ('Trace:  [TgtkObject.IntSendMessage3] %s --> got handle=nil',[Sender.ClassName]));
    Case LM_Message of

      LM_SetText  : SetText(PgtkWidget(Handle), Data); 

      LM_AddChild :
      begin
        Assert(False, 'Trace:Adding a child to Parent');
        If (TWinControl(Sender).Parent is TToolbar) then
        Begin
          //  Assert(False, Format('Trace:  [TgtkObject.IntSendMessage3] %s --> %s   ---calling INSERTBUTTON from Add Child', [AParent.ClassName, Sender.ClassNAme]));
          exit;
        end
        else Begin
          AParent := (Sender as TWinControl).Parent;
          if Not Assigned(AParent) then Begin
            Assert(true, Format('Trace: [TgtkObject.IntSendMessage3] %s --> Parent is not assigned', [Sender.ClassName]));
          end
          else Begin
            Assert(False, Format('Trace:  [TgtkObject.IntSendMessage3] %s --> Calling Add Child: %s', [AParent.ClassName, Sender.ClassNAme]));
            AddChild(Pgtkwidget(AParent.Handle), PgtkWidget(Handle), AParent.Left, AParent.Top);
          end;
        end;
      end;
      
      LM_LV_DELETEITEM :
      begin
        if (Sender is TListView) then
        begin
          Num := Integer(data^);
          Widget:= GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget;
          gtk_clist_remove(PgtkCList(Widget),Num);
        end;
      end;
      
      LM_LV_CHANGEITEM :
        if (Sender is TListView) then
          ListViewChangeItem(Sender,Integer(data^));

      LM_LV_ADDITEM :
      if (Sender is TListView) then
      begin
        ListViewAddItem(Sender);
        ListViewChangeItem(Sender,TListView(Sender).Items.Count-1);
      end;

      LM_LV_SELECTITEM:
      if (Sender is TListView) then
      begin
        Widget:= GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget;
        gtk_clist_unselect_all(PGtkCList(Widget));
        if Data<>nil then
          gtk_clist_select_row(PGtkCList(Widget),TListItem(Data).Index,0);
      end;
      
      LM_BRINGTOFRONT:
      begin
{         Assert(False, 'Trace:TODO:bringtofront');
         //For now just hide and show again.
	 if (Sender is TControl) then begin
	   TControl(Sender).Parent.RemoveControl(TControl(Sender));
	   writeln('Removed control ', TControl(Sender).Name);
	   TControl(Sender).Parent.InsertControl(TControl(Sender));
	   writeln('Inserted control ', TControl(Sender).Name);
    end;
    }
        if (Sender is TCustomForm) then
          BringFormToFront(Sender);
      end;

      LM_BTNDEFAULT_CHANGED :
      Begin
        if (TButton(Sender).Default) then begin
        writeln('AAA1 ',TButton(Sender).Name,':',TButton(Sender).ClassName);
          TButton(Sender).SetFocus;
          //and (GTK_WIDGET_CAN_DEFAULT(pgtkwidget(handle))) then
          //  gtk_widget_grab_default(pgtkwidget(handle))
          //else
          //  gtk_widget_Draw_Default(pgtkwidget(Handle));  //this isn't right but I'm not sure what to call
        end;
      end;

      LM_DESTROY :
      begin
        DestroyLCLControl(Sender);
      end;

      LM_DRAGINFOCHANGED :
      Begin
        (*
        if ((Sender is TEdit) and((Sender as TEdit).DragMode = dmAutoMatic)) then
           Begin
           //drag and drop
           gtk_drag_dest_set (p,
                     GTK_DEST_DEFAULT_ALL,
                     target_table, TargetEntrys - 1,
                     GDK_ACTION_COPY or GDK_ACTION_MOVE);
            gtk_signal_connect( PgtkObject(p), 'drag_data_received',
                      TGTKSignalFunc( @edit_drag_data_received), Sender);

            gtk_drag_source_set (p, GDK_BUTTON1_MASK,
                       target_table, TargetEntrys,
                       GDK_ACTION_COPY or GDK_ACTION_MOVE);
            gtk_drag_source_set_icon (p,
                            gtk_widget_get_colormap (pgtkwidget(p)),
                            drag_icon, drag_mask);
            gtk_signal_connect (GTK_OBJECT (p), 'drag_data_get',
                      GTK_SIGNAL_FUNC (@Edit_source_drag_data_get), Sender);
            gtk_signal_connect (GTK_OBJECT (p), 'drag_data_delete',
                      GTK_SIGNAL_FUNC (@Edit_source_drag_data_delete), Sender);
            end
            else
           Begin
           //drag and drop
           gtk_drag_dest_set (p,
                     GTK_DEST_DEFAULT_ALL,
                     target_table, TargetEntrys - 1,
                     GDK_ACTION_COPY or GDK_ACTION_MOVE);
            gtk_signal_connect( PgtkObject(p), 'drag_data_received',
                      TGTKSignalFunc( @edit_drag_data_received), Sender);

            gtk_drag_source_set (p, GDK_BUTTON1_MASK,
                       target_table, TargetEntrys,
                       GDK_ACTION_COPY or GDK_ACTION_MOVE);
            gtk_drag_source_set_icon (p,
                            gtk_widget_get_colormap (pgtkwidget(p)),
                            drag_icon, drag_mask);
            gtk_signal_connect (GTK_OBJECT (p), 'drag_data_get',
                      GTK_SIGNAL_FUNC (@Edit_source_drag_data_get), Sender);
            gtk_signal_connect (GTK_OBJECT (p), 'drag_data_delete',
                      GTK_SIGNAL_FUNC (@Edit_source_drag_data_delete), Sender);
            end;
          *)
      end;

      //TBitBtn
      LM_IMAGECHANGED, LM_LAYOUTCHANGED :
      begin
         box1 := gtk_object_get_data(pgtkObject(handle),'HBox');
         if box1 <> nil then
         begin
            gtk_container_remove(PgtkContainer(box1),
              gtk_object_get_data(pgtkObject(handle),'Label'));
            PixMapWid:=gtk_object_get_data(pgtkObject(handle),'Pixmap');
            if PixMapWid<>nil then
              gtk_container_remove(PgtkContainer(box1),PixMapWid);
            gtk_container_remove(PgtkContainer(handle),box1);
            // gtk_container_remove automatically destroys box1 if ref count=0
            //  so we dont need gtk_widget_destroy(box1);
         end;

         pixmap := pgdkPixmap(
                     PgdiObject(TBitBtn(Sender).Glyph.Handle)^.GDIBitmapObject);
         if (TBitBtn(Sender).Glyph.Width>1)
         or (TBitBtn(Sender).Glyph.Height>1) then begin
           if PgdiObject(TBitBtn(Sender).Glyph.handle)^.GDIBitmapMaskObject <> nil
           then begin
             PixMapWid := gtk_pixmap_new(pixmap,
               PgdiObject(TBitBtn(Sender).Glyph.handle)^.GDIBitmapMaskObject)
           end else begin
             PixMapWid := gtk_pixmap_new(pixmap,nil);
           end;
         end else begin
           PixMapWid:=nil;
         end;

         pStr := Ampersands2Underscore(PChar(TBitBtn(Sender).Caption));
         try
           pLabel := gtk_label_new(pStr);
           Accel:= gtk_label_parse_uline(PGtkLabel(pLabel), pStr);
           
           Accelerate(TBitBtn(Sender),PGtkWidget(TBitBtn(Sender).Handle),
             Accel,0,'clicked');
         finally
           StrDispose(pStr);
         end;  

         if (TBitBtn(Sender).Layout = blGlyphLeft)
         or (TBitBtn(Sender).Layout = blGlyphRight) then
         Begin
           box1 := gtk_hbox_new(False,0);
         end
         else Begin
           box1 := gtk_vbox_new(False,0);
         end;

         if (TBitBtn(Sender).Layout = blGlyphLeft)
         or (TBitBtn(Sender).Layout = blGlyphTop) then
         begin
           if PixMapWid<>nil then
             gtk_box_pack_start(pGTKBox(Box1),PixMapWid,false,false,
               TBitBtn(Sender).Spacing);
           gtk_box_pack_start(pGTKBox(Box1),pLabel,PixMapWid=nil,PixMapWid=nil,
             TBitBtn(Sender).Spacing);
         end
         else begin
           gtk_box_pack_start(pGTKBox(Box1),pLabel,PixMapWid=nil,PixMapWid=nil,
             TBitBtn(Sender).Spacing);
           if PixMapWid<>nil then
             gtk_box_pack_start(pGTKBox(Box1),PixMapWid,False,False,
               TBitBtn(Sender).Spacing);
         end;
         gtk_object_set_data(pgtkObject(handle),'HBox',Box1);
         gtk_object_set_data(pgtkObject(handle),'Label',pLabel);
         gtk_object_set_data(pgtkObject(handle),'Pixmap',PixMapWid);

         if PixMapWid<>nil then
           gtk_widget_show(pixmapwid);
         gtk_widget_show(pLabel);
         gtk_container_add(PgtkContainer(handle),box1);
         gtk_widget_show(box1);
      end;

//SH: think of TBitmap.handle!!!!
     LM_LOADXPM:
         Begin
           if (sender is TBitmap) then
             If LowerCase(ExtractFileExt(String(PChar(Data)))) = '.xpm' then
             Begin
               LoadFromXPMFile(TBitmap(Sender),PChar(data));
             end
             else
               LoadFromPixbufFile(TBitmap(Sender),PChar(data));
         end;


      LM_POPUPSHOW :
        Begin 
          gtk_menu_popup(PgtkMenu(TPopupMenu(Sender).Handle),
                         nil,
                         nil,
                         nil,
                         nil,
                         0,
                         0);
          {Displays a menu and makes it available for selection. Applications
          can use this function to display context-sensitive menus, and will
          typically supply NULL for the parent_menu_shell, parent_menu_item,
          func and data parameters.
          The default menu positioning function will position the menu at the
          current pointer position.
          menu :	a GtkMenu.
          parent_menu_shell: the menu shell containing the triggering menu item.
          parent_menu_item: the menu item whose activation triggered the popup.
          func :	a user supplied function used to position the menu.
          data :	user supplied data to be passed to func.
          button :	the button which was pressed to initiate the event.
          activate_time :	the time at which the activation event occurred.
          }

        end;

      LM_SETFILTER :
      begin 
        { ToDo:
        if Sender is TFileDialog then begin
          pStr := StrAlloc(length(TFileDialog(Sender).Filter) + 1); 
          try
            StrPCopy(pStr, TFileDialog(Sender).Filter); 
            gtk_file_selection_complete(PGtkFileSelection(Handle), pstr);
          finally
            StrDispose(pStr); 
          end;
        end;}
      end; 
 
      LM_SETFILENAME : 
      begin 
        if Sender is TFileDialog then begin
          pStr := StrAlloc(length(TFileDialog(Sender).Filename) + 1); 
          try
            StrPCopy(pStr, TFileDialog(Sender).Filename); 
            gtk_file_selection_set_filename( PGtkFileSelection(Handle), pStr);
          finally
            StrDispose(pStr); 
          end;
        end;
      end; 

      LM_SETFOCUS:
      begin
        writeln('[TgtkObject.IntSendMessage3] LM_SETFOCUS ',TObject(Sender).ClassName);
        SetFocus(Handle);
      end;

      LM_SetSize :
      begin
        Assert(False, Format('Trace:  [TgtkObject.IntSendMessage3] %s --> LM_SetSize(%d, %d, %d, %d)', [Sender.ClassNAme, PRect(Data)^.Left,PRect(Data)^.Top,PRect(Data)^.Right,PRect(Data)^.Bottom]));
        //writeln('[IntSendMessage3.lm_setsize] Left=',PRect(Data)^.Left,' Top=',PRect(Data)^.Top,
        //  ' Right=',PRect(Data)^.Right,' Bottom=',PRect(Data)^.Bottom);
        //writeln('[LM_SetSize] A ',Sender.ClassName,' ',PgtkWidget(Handle)^.window<>nil);
        ResizeChild(Sender,PRect(Data)^.Left,PRect(Data)^.Top,
             PRect(Data)^.Right,PRect(Data)^.Bottom);
        //writeln('[LM_SetSize] B ',Sender.ClassName,' ',PgtkWidget(Handle)^.window<>nil);
      end;

      LM_ShowModal :
      begin
        if Sender is TCommonDialog then
        begin
          // Should be done elsewhere (eg via SetLabel) not here!
          gtk_window_set_title(PGtkWindow(handle),
                               PChar(TCommonDialog(Sender).Title));
          if Sender is TColorDialog then
            SetColorDialogColor(Pointer(Handle),TColorDialog(Sender).Color);
        end;

        if (Sender is TCustomForm) then begin
          gtk_window_set_default_size(PgtkWindow(handle),
                                TControl(Sender).Width,TControl(Sender).Height);
          gtk_widget_set_uposition(PgtkWidget(handle),
                                   TControl(Sender).Left, TControl(Sender).Top);
        end else begin
          gtk_window_set_position(PGtkWindow(handle), GTK_WIN_POS_CENTER);
        end;
        UnsetResizeRequest(PGtkWidget(Handle));

        // Try to make all Modal forms transient to the main form
        If Sender is TCustomForm then begin
          //If this is a Form, aka GTKWindow, try
          //and keep decoration and Functions -

          //apart from none and sizeable, this will
          //only work if WM supports motif flags
          //properly, which very few actually do.

          If (Application.MainForm<>nil)
          and (Sender <> Application.MainForm)
          and Application.MainForm.HandleAllocated then begin
            //gtk_window_set_transient_for(PGtkWindow(handle),
            //  PGtkWindow(Application.MainForm.handle));
          end;
        end;
        gtk_window_set_modal(PGtkWindow(handle), true);
        gtk_widget_show(PGtkWidget(handle));
      end;

      LM_TB_BUTTONCOUNT:
      begin
         if (Sender is TToolbar)
           then Result := pgtkToolbar(handle)^.num_Children
           else Result := -1;
      end;

//SH: think of TCanvas.handle!!!!
      LM_REDRAW: 
      begin 
        Assert(False, Format('Trace:  [TgtkObject.IntSendMessage3] %s --> Redraw', [Sender.ClassName]));
        if (Sender is TCanvas) then 
          ReDraw(PgtkWidget(TCanvas(Sender).Handle))
        else begin
          if Sender is TWinControl then
            ReDraw(PgtkWidget(Handle))
          else begin
            Rect := TControl(Sender).BoundsRect;
            InvalidateRect(TControl(Sender).Parent.Handle, @Rect, true);
          end;  
        end; 
      end;

      LM_AddPage :
        if Sender is TCustomNoteBook then begin
          AddNBPage(TControl(Sender), TLMNotebookEvent(Data^).Child,
            TLMNotebookEvent(Data^).Page);
        end;

      LM_RemovePage :
        if Sender is TCustomNoteBook then begin
          RemoveNBPage(TControl(Sender), TLMNotebookEvent(Data^).Page);
        end;

      LM_MovePage :
        if Sender is TCustomNoteBook then begin
          MoveNBPage(TControl(Sender), TLMNotebookEvent(Data^).Child,
            TLMNotebookEvent(Data^).Page);
        end;

      LM_ShowTabs :
      begin
        gtk_notebook_set_show_tabs(PGtkNotebook(Handle),
          Boolean(Integer(TLMNotebookEvent(Data^).ShowTabs)));
      end;

      LM_SetTabPosition :
      begin
        case TTabPosition(TLMNotebookEvent(Data^).TabPosition^) of
          tpTop   : gtk_notebook_set_tab_pos(PGtkNotebook(Handle), GTK_POS_TOP);
          tpBottom: gtk_notebook_set_tab_pos(PGtkNotebook(Handle), GTK_POS_BOTTOM);
          tpLeft  : gtk_notebook_set_tab_pos(PGtkNotebook(Handle), GTK_POS_LEFT);
          tpRight : gtk_notebook_set_tab_pos(PGtkNotebook(Handle), GTK_POS_RIGHT);
        end;
      end;


      LM_INSERTTOOLBUTTON:
      begin
      Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!!!!!!!!');
      Assert(False, 'Trace:Toolbutton being inserted');
      Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!!!!!!!!');
         If (SENDER is TWINCONTROL) Then
         Begin
            pStr := StrAlloc(Length(TToolbutton(SENDER).Caption)+1);
            try
              StrPCopy(pStr,TToolbutton(SENDER).Caption);
              pStr2 := StrAlloc(Length(TControl(Sender).Hint)+1);
            finally
              StrPCopy(pStr2,TControl(Sender).Hint);
            end;
         end 
         else Begin
            raise Exception.Create('Can not assign this control to the toolbar');
            exit;
         end;
    
         num := TToolbar(TWinControl(Sender).parent).Buttonlist.IndexOf(TControl(Sender));
         if num < 0 then Num := TToolbar(TWinControl(Sender).parent).Buttonlist.Count+1;
         Assert(False, Format('Trace:NUM = %d in INSERTBUTTON',[num]));

         {Make sure it's created!!}
         if handle = 0 
            then IntSendMessage3(LM_CREATE,Sender,nil);

         gtk_toolbar_insert_widget(pGTKToolbar(TWinControl(sender).parent.Handle),
              pgtkwidget(handle),pstr,pStr2,Num);
         StrDispose(pStr);
         StrDispose(pStr2);
      Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!!!!!!!!');
      end;

      LM_DELETETOOLBUTTON:
      Begin
         with pgtkToolbar(TToolbar(TWinControl(Sender).parent).handle)^ do
            children := g_list_remove(pgList(children), sender);
// Next 3 lines: should be same as above, remove when above lines are proofed
//         pgtkToolbar(TToolbar(TWinControl(Sender).parent).handle)^.children := 
//            g_list_remove(pgList(pgtkToolbar(TToolbar(TWinControl(Sender).parent).handle)^.children),
//               sender);
      end;

      LM_Invalidate : 
      begin 
         Assert(False, 'Trace:Trying to invalidate window... !!!');
   //THIS DOESN'T WORK YET....
{
         Event.thetype := GDK_EXPOSE;
     Event.window := PgtkWidget(Handle)^.Window;
     Event.Send_Event := 0;
     Event.X := 0;
     Event.Y := 0;
     Event.Width := PgtkWidget((Handle)^.Allocation.Width;
     Event.Height := PgtkWidget(Handle)^.Allocation.Height;
         gtk_Signal_Emit_By_Name(PgtkObject(Handle),'expose_event',[(Sender as TWinControl).Handle,Sender,@Event]);
     Assert(False, 'Trace:Signal Emitted - invalidate window');
}
        gtk_widget_queue_draw(PGtkWidget(Handle));
      end; 
      
      LM_SETFORMICON :
      begin
        if Sender is TCustomForm then begin
          if (Handle<>0) and (Data<>nil) then begin
            FormIconGdiObject:=Data;
            //writeln('LM_SETFORMICON ',FormIconGdiObject<>nil,' ',pgtkWidget(Handle)^.Window<>nil);
            if (FormIconGdiObject<>nil) then begin
              AWindow:=GetControlWindow(PGtkWidget(Handle));
              if AWindow<>nil then begin
                gdk_window_set_icon(AWindow, nil,
                  FormIconGdiObject^.GDIBitmapObject,
                  FormIconGdiObject^.GDIBitmapMaskObject);
              end;
            end;
          end;
        end;
      end;

      LM_SCREENINIT :
      begin
        { Compute pixels per inch variable }
        PLMScreenInit(Data)^.PixelsPerInchX:= 
          Round(gdk_screen_width / (gdk_screen_width_mm / 25.4));
        PLMScreenInit(Data)^.PixelsPerInchY:= 
          Round(gdk_screen_height / (gdk_screen_height_mm / 25.4));
        PLMScreenInit(Data)^.ColorDepth:= gdk_visual_get_system^.depth;
      end;  

      LM_GETITEMS :
      begin
        case TControl(Sender).fCompStyle of
          csComboBox :
            Result:=longint(gtk_object_get_data(PGtkObject(Handle),'LCLList'));
            
          csCListBox:
            begin
              Widget:= GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget;

              Data := TGtkCListStringList.Create(PGtkCList(Widget));
              Result := integer(Data);
            end;
            
          csListBox :
            begin
              Widget:= GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget;
              Data:= TGtkListStringList.Create(PGtkList(Widget),
                                               TWinControl(Sender));
              Result:= Integer(Data);
            end;
        else 
          raise Exception.Create('Message LM_GETITEMS - Not implemented');
        end;
      end;

      LM_GETTEXT :
      begin
        Result := Integer(GetText(Sender As TComponent,PString(Data)^));
      end;

      LM_GETITEMINDEX :
      begin
        case TControl(Sender).fCompStyle of
           csComboBox:
             Result:=GetComboBoxItemIndex(TComboBox(Sender));

           csListBox:
             begin
               if Handle<>0 then begin
                 if TListBox(Sender).MultiSelect then 
                   Widget:= PGtkList(GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget)^.last_focus_child
                 else begin
                   GList:= PGtkList(GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget)^.selection;
                   if GList = nil then
                     Widget:= nil
                   else
                     Widget:= PGtkWidget(GList^.data);
                 end;  
                 if Widget = nil then Result:= -1
                 else Result:= gtk_list_child_position(PGtkList(GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget), Widget);
               end else
                 Result:=-1;
             end;
             
           csCListBox:
             begin
               GList:= PGtkCList(GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget)^.selection;
               if GList = nil then
                 Result := -1
               else
                 Result := integer(GList^.Data);
             end;
             
           csNotebook:
             begin
               TLMNotebookEvent(Data^).Page := 
                 gtk_notebook_get_current_page(PGtkNotebook(Handle));
               UpdateNoteBookClientWidget(Sender);
             end;
             
        end;
      end;
          
      LM_SETITEMINDEX :
        if Handle<>0 then begin
          case TControl(Sender).fCompStyle of
        
          csComboBox:
            SetComboBoxItemIndex(TComboBox(Sender),Integer(Data));

          csListBox :
            gtk_list_select_item(PGtkList(GetWidgetInfo(Pointer(Handle),
                                   True)^.ImplementationWidget), Integer(Data));

          csCListBox:
            gtk_clist_select_row(PGtkCList(GetWidgetInfo(Pointer(Handle),
                                   True)^.ImplementationWidget),
                                 Integer(Data), 1);    // column
              
          csNotebook:
            if Data<>nil then begin
              gtk_notebook_set_page(PGtkNotebook(Handle),
                                    TLMNotebookEvent(Data^).Page);
              UpdateNoteBookClientWidget(Sender);
            end;
          end;
        end;
          
      LM_GETSELSTART :
      begin
        if (Sender is TControl) then begin
          case TControl(Sender).fCompStyle of
          csComboBox:
            Widget:= PGtkWidget(PGtkCombo(Handle)^.entry);
          csEdit, csMemo:
  	    Widget:= GetWidgetInfo(Pointer(Handle), true)^.ImplementationWidget;
	  else
	    Widget:= nil;  
          end;
	  if Widget <> nil then begin
            if PGtkEditable(Widget)^.selection_start_pos < PGtkEditable(Widget)^.selection_end_pos then
              Result:= PGtkEditable(Widget)^.selection_start_pos
	    else
	      Result:= PGtkEditable(Widget)^.selection_end_pos
          end else Result:= 0;
        end;
      end;
          
      LM_GETSELLEN :
      begin
        if (Sender is TControl) then begin
          case TControl(Sender).fCompStyle of
          csComboBox:
            Result:= Abs(PGtkEditable(PGtkCombo(Handle)^.entry)^.selection_end_pos
                     - PGtkEditable(PGtkCombo(Handle)^.entry)^.selection_start_pos);
          csEdit, csMemo:
	    begin
	      Widget:= GetWidgetInfo(Pointer(Handle), true)^.ImplementationWidget;
              Result:= Abs(PGtkEditable(Widget)^.selection_end_pos - PGtkEditable(Widget)^.selection_start_pos);
	    end;
          end;
        end;
      end;
  
      LM_GETLIMITTEXT :
      begin
        if (Sender is TControl)
        and (TControl(Sender).fCompStyle = csComboBox) then
        begin
          Result:= PGtkEntry(PGtkCombo(Handle)^.entry)^.text_max_length;
        end;
      end;
  
      LM_SETSELSTART : 
      begin
        if (Sender is TControl) then begin
          case TControl(Sender).fCompStyle of
          csComboBox:
            gtk_editable_set_position(PGtkEditable(PGtkCombo(Handle)^.entry),
                                      Integer(Data));
	  csEdit, csMemo: 
	    begin
	      Widget:= GetWidgetInfo(Pointer(Handle), true)^.ImplementationWidget;
              gtk_editable_set_position(PGtkEditable(Widget), Integer(Data));
	    end;  
	  end;
        end;
      end;
          
      LM_SETSELLEN :
      begin
        if (Sender is TControl) then begin
          case TControl(Sender).fCompStyle of
  	    csComboBox:
            begin
              gtk_editable_select_region(PGtkEditable(PGtkCombo(Handle)^.entry),
                gtk_editable_get_position(PGtkEditable(PGtkCombo(Handle)^.entry)),
                gtk_editable_get_position(PGtkEditable(PGtkCombo(Handle)^.entry))
                          + Integer(Data));
            end;
            
	        csEdit:
            begin
              gtk_editable_select_region(PGtkEditable(Handle),
                gtk_editable_get_position(PGtkEditable(Handle)),
                gtk_editable_get_position(PGtkEditable(Handle)) + Integer(Data));
            end;
      	  end;
      	end;
      end;  
 
      LM_GetLineCount :
      begin

      end;

      LM_GETSELCOUNT :
      begin
        case (Sender as TControl).fCompStyle of
          csListBox :
            Result:=g_list_length(PGtkList(GetWidgetInfo(Pointer(Handle),
                               True)^.ImplementationWidget)^.selection);
          csCListBox:
            Result:= g_list_length(PGtkCList(GetWidgetInfo(Pointer(Handle),
                               True)^.ImplementationWidget)^.selection);
        end;
      end;
  
      LM_GETSEL :
      begin
        if (Sender as TWinControl).fCompStyle = csListBox then
        begin
          { Get the child in question of that index }
          ListItem:= g_list_nth_data(PGtkList(GetWidgetInfo(Pointer(Handle),
                       True)^.ImplementationWidget)^.children, Integer(Data^));
          Result:= g_list_index(PGtkList(GetWidgetInfo(Pointer(Handle),
                            True)^.ImplementationWidget)^.selection, ListItem);
        end
        else if (Sender as TControl).fCompStyle = csCListBox then
        begin
          { Get the selections }
          GList:= PGtkCList(GetWidgetInfo(Pointer(Handle),
                                       True)^.ImplementationWidget)^.selection;
          Result := -1;      { assume: nothing found }
          while Assigned(GList) do begin
            if integer(GList^.data) = integer(Data^) then begin
              Result:= 0;
              Break;
            end else
              GList := GList^.Next;
          end;
        end;
      end;
  
      LM_SETLIMITTEXT :
      begin
         if (Sender is TControl) and (TControl(Sender).fCompStyle = csComboBox) 
         then gtk_entry_set_max_length(PGtkEntry(PGtkCombo(Handle)^.entry),
                                       Integer(Data^));
      end;
 
      LM_SORT :
      begin
         if  (Sender is TControl) and assigned (data) then
         begin
            case TControl(Sender).fCompStyle of
               csComboBox, 
               csListBox   : TGtkListStringList(TLMSort(Data^).List).Sorted:=
                                  TLMSort(Data^).IsSorted;
               csCListBox  : TGtkCListStringList(TLMSort(Data^).List).Sorted :=
                                  TLMSort(Data^).IsSorted;
            end
         end
      end;
 
      LM_SETSEL :
      begin
        if (Sender is TControl)
        and (TControl(Sender).fCompStyle in [csListBox, csCListBox])
        and assigned (data) then
        begin
          if (TControl(Sender).fCompStyle = csListBox) then
          begin
            if TLMSetSel(Data^).Selected then begin
              gtk_list_select_item(
                PGtkList(GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget),
                TLMSetSel(Data^).Index);
            end else
              gtk_list_unselect_item(
                PGtkList(GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget),
                TLMSetSel(Data^).Index);
          end
          else if (TControl(Sender).fCompStyle = csCListBox) then
          begin
            if TLMSetSel(Data^).Selected then
              gtk_clist_select_row(
                PGtkCList(GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget),
                TLMSetSel(Data^).Index, 0)
            else
              gtk_clist_unselect_row(
                PGtkCList(GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget),
                TLMSetSel(Data^).Index, 0);
          end;
        end;
      end;
 
      LM_SETSELMODE :
      begin
         if (Sender is TControl) and 
            (TControl(Sender).fCompStyle in [csListBox, csCListBox]) and
            assigned (data) then 
         begin
            if TLMSetSelMode(Data^).MultiSelect then 
            begin
               if TLMSetSelMode(Data^).ExtendedSelect 
                  then SelectionMode:= GTK_SELECTION_EXTENDED
                  else SelectionMode:= GTK_SELECTION_MULTIPLE;
            end 
            else 
              SelectionMode:= GTK_SELECTION_BROWSE;
            case TControl(Sender).fCompStyle of
              csListBox  : gtk_list_set_selection_mode(PGtkList(              
                GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget), SelectionMode);
              csCListBox : gtk_clist_set_selection_mode(PGtkCList(
                GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget),SelectionMode);
            else
              Assert (true, 'WARNING:[TgtkObject.IntSendMessage3] usage of LM_SETSELMODE unimplemented for actual component');
            end;
         end;
      end;
 
      LM_SETBORDER :
      begin
        if (Sender is TControl) then
        begin
          if (TControl(Sender).fCompStyle = csListBox) then
            begin
              { In TempWidget, a viewport is stored }
              Widget:= PGtkWidget(PGtkBin(Handle)^.child);
              if TListBox(Sender).BorderStyle = TBorderStyle(bsSingle)
              then
                gtk_viewport_set_shadow_type(PGtkViewPort(Widget), GTK_SHADOW_IN)
              else
                gtk_viewport_set_shadow_type(PGtkViewPort(Widget), GTK_SHADOW_NONE);
            end
            else if TControl(Sender).fCompStyle = csCListBox then
            begin
              if TListBox(Sender).BorderStyle = TBorderStyle(bsSingle)
              then
                gtk_viewport_set_shadow_type(
                  PGtkViewPort(PGtkBin(Handle)^.Child), GTK_SHADOW_NONE)
              else
                gtk_viewport_set_shadow_type(
                  PGtkViewPort(PGtkBin(Handle)^.Child), GTK_SHADOW_IN);
            end;
         end;
      end;
      
      LM_SETSHORTCUT :
      begin
        with TLMShortcut(data^) do begin
          Widget:= PGtkWidget(Handle);
        end;
        if Sender is TControl then begin
          case TControl(Sender).fCompStyle of
            csBitBtn,
            csButton,
            csToolButton,
            csRadioButton,
            csCheckBox:
              // ToDo: use accelerator group of Form
              Accelerate(TComponent(Sender), Widget, TLMShortcut(data^), 'clicked');
            else
              // ToDo: use accelerator group of Form
              Accelerate(TComponent(Sender), Widget, TLMShortcut(data^), 'activate_item');
          end;
        end else if Sender is TMenuItem then begin
          Accelerate(TComponent(Sender), Widget, TLMShortcut(data^),
            'activate_item');
        end;
      end;

      LM_SETGEOMETRY :
      begin
        if Sender is TWinControl then begin
          Widget:= PGtkWidget(TWinControl(Sender).Handle);
          if Widget <> nil then begin
            with Geometry, TControl(Sender) do begin
      	      if Constraints.MinWidth > 0 then
                min_width:= Constraints.MinWidth else min_width:= 1;
      	      if Constraints.MaxWidth > 0 then
                max_width:= Constraints.MaxWidth else max_width:= 32767;
      	      if Constraints.MinHeight > 0 then
                min_height:= Constraints.MinHeight else min_height:= 1;
      	      if Constraints.MaxHeight > 0 then
                max_height:= Constraints.MaxHeight else max_height:= 32767;
      	      base_width:= Width;
      	      base_height:= Height;
      	      width_inc:= 1;
      	      height_inc:= 1;
      	      min_aspect:= 0;
      	      max_aspect:= 1;
      	    end;
            gtk_window_set_geometry_hints(PGtkWindow(Widget), nil, @Geometry,
                                        GDK_HINT_MIN_SIZE or GDK_HINT_MAX_SIZE);
          end;
        end;
      end;

      else
        if Sender<>nil then
          Assert(True, Format('WARNING: Unhandled message %d in IntSendMessage3'
             +'send by %s --> message:Redraw', [LM_Message, Sender.ClassName]));
         // unhandled message
      end; // end of 2nd case
    end;   // end of else-part of 1st case
  end;   // end of 1st case
end;

{------------------------------------------------------------------------------
  Function: TGtkObject.GetText
  Params:  Sender: The control to retrieve the text from
  Returns: the requested text

  Retrieves the text from a gtk control. this is a replacement for
  the LM_GetText message.
 ------------------------------------------------------------------------------}
function TGtkObject.GetText(Sender: TComponent; var Text: String): Boolean;
var
  CS: PChar;
begin
  Result := True;
  case TControl(Sender).fCompStyle of
   csComboBox:
     begin
       Text := StrPas(gtk_entry_get_text(PGtkEntry(PGtkCombo(
                                         TComboBox(Sender).Handle)^.entry)));
     end;
     
   csEdit: Text:= StrPas(gtk_entry_get_text(PgtkEntry(TWinControl(Sender).Handle)));

   csMemo    : begin
                  CS := gtk_editable_get_chars(PGtkEditable(
                    GetWidgetInfo(Pointer(TWinControl(Sender).Handle), True)^.ImplementationWidget), 0, -1);
                  Text := StrPas(CS);
                  g_free(CS);
               end;
  else
    Result := False;
  end;
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.ResizeChild

  Params:  sender - the object which invoked this function
           Left,Top,Width,Height - new dimensions for the control
  Returns: Nothing

  *Note: Resize a child widget on the parents fixed widget
 ------------------------------------------------------------------------------}
procedure TgtkObject.ResizeChild(Sender : TObject;
  Left, Top, Width, Height : Integer);
var
  Widget: PGtkWidget;
begin
  //writeln('[TgtkObject.ResizeChild] START ',TControl(Sender).Name,':',Sender.Classname,' Left=',Left,' Top=',Top,' Width=',Width,' Height=',Height);
  Assert(false, (Format('trace:  [TgtkObject.ResizeChild] %s --> Resize', [Sender.ClassNAme])));

  if Sender is TWinControl then begin
    if TWinControl(Sender).HandleAllocated then begin
      Widget := pgtkWidget(TWinControl(Sender).Handle);
      SetResizeRequest(Widget);
      {$IFDEF VerboseClientRectBugFix}
      if (Sender is TCustomForm) then
        writeln(' FFF ResizeChild ',Sender.ClassName,' ',Left,',',Top);
      {$ENDIF}
    end;
  end;
  //writeln('[TgtkObject.ResizeChild] END ',Sender.Classname,' Left=',Left,' Top=',Top,' Width=',Width,' Height=',Height);
end;


{------------------------------------------------------------------------------
  Method: TGtkObject.AddChild
  Params:  parent - 
           child  -
           left, top -
  Returns: Nothing

  *Note: Adds A Child to a Parent Widget
 ------------------------------------------------------------------------------}
procedure TgtkObject.AddChild(Parent,Child : Pointer; Left,Top: Integer);
var
  pFixed: PGTKWidget;
begin
  pFixed := GetFixedWidget(PGtkWidget(Parent));
  if pFixed <> Parent then begin
    // parent changed for child
    FixedPutControl(pFixed, Child, Left, Top);
    RegroupAccelerator(Child);
  end;
end;


{------------------------------------------------------------------------------
  Method: TGtkObject.SetText
  Params:  Child -
           data  -
  Returns: Nothing

  Sets the text of a control.

  WARNING: This should possibly be merged with the SetLabel method!
           It's only left in here for TStatusBar right now cause it
           may be nice to use it with different panels.
 ------------------------------------------------------------------------------}
procedure TgtkObject.SetText(Child, Data: Pointer);
type
  pMsg = ^TLMSetControlText;
var
  num : Integer;
begin
  case pMsg(Data)^.fCompStyle of
  csStatusBar :
    begin
      num := gtk_statusbar_get_context_id(PGTKStatusBar(Child),PChar(inttostr(pMsg(Data)^.panel)));
      gtk_statusbar_push(PGTKStatusBar(Child),num,pMsg(Data)^.Userdata);
    end
  else
    writeln ('STOPPOK: [TGtkObject.SetText] Possible superfluous use of SetText, use SetLabel instead!');
  end;
{STOPPOK: Code seems superfluous, see SetLabel instead}
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.SetLabel
  Params:  sender - the calling object
           data   - String (PChar) to be set as label for a control
  Returns: Nothing

  Sets the label text on a widget
 ------------------------------------------------------------------------------}
procedure TgtkObject.SetLabel(Sender : TObject; Data : Pointer);

  procedure SetNotebookPageTabLabel;
  var
    NoteBookWidget: PGtkWidget; // the notebook
    PageWidget: PGtkWidget;     // the page (content widget)
    TabWidget: PGtkWidget;      // the tab (hbox containing a pixmap, a label
                                //          and a close button)
    TabLabelWidget: PGtkWidget; // the label in the tab
    MenuWidget: PGtkWidget;     // the popup menu (hbox containing a pixmap and
                                // a label)
    MenuLabelWidget: PGtkWidget; // the label in the popup menu item
    NewText: PChar;
  begin
    // dig through the hierachy to get the labels
    NoteBookWidget:=PGtkWidget((TControl(Sender).Parent).Handle);
    PageWidget:=PGtkWidget(TWinControl(Sender).Handle);
    TabWidget:=gtk_notebook_get_tab_label(PGtkNoteBook(NotebookWidget),
                                          PageWidget);
    if TabWidget<>nil then
      TabLabelWidget:=gtk_object_get_data(PGtkObject(TabWidget), 'TabLabel')
    else
      TabLabelWidget:=nil;
    MenuWidget:=gtk_notebook_get_menu_label(PGtkNoteBook(NotebookWidget),
                                            PageWidget);
    if MenuWidget<>nil then
      MenuLabelWidget:=gtk_object_get_data(PGtkObject(MenuWidget), 'TabLabel')
    else
      MenuLabelWidget:=nil;
    // set new text
    NewText:=PChar(Data);
    if TabLabelWidget<>nil then
      gtk_label_set_text(pGtkLabel(TabLabelWidget), NewText);
    if MenuLabelWidget<>nil then
      gtk_label_set_text(pGtkLabel(MenuLabelWidget), NewText);
  end;
  
  procedure SetMenuItemCaption;
  var
    MenuItemWidget: PGtkWidget;
    MenuItem: TMenuItem;
  begin
    MenuItem:=TMenuItem(Sender);
    if not MenuItem.HandleAllocated then exit;
    MenuItemWidget:=PGtkWidget(MenuItem.Handle);
    SetMenuItemLabelText(MenuItem,MenuItemWidget);
  end;
  

var
  DC : hDC;
  P : Pointer;
  aLabel, pLabel: pchar;
  AccelKey : integer;
begin
  if Sender is TMenuItem then begin
    SetMenuItemCaption;
    exit;
  end;

  if Sender is TWinControl
  then Assert(False, Format('Trace:  [TgtkObject.SetLabel] %s --> label %s', [Sender.ClassName, TControl(Sender).Caption]))
  else begin
    Assert(False, Format('Trace:WARNING: [TgtkObject.SetLabel] %s --> No Decendant of TWinControl', [Sender.ClassName]));
    raise Exception.Create('[TgtkObject.SetLabel] ERROR: Sender ('+Sender.Classname+')'
        +' is not TWinControl ');
  end;

  P := Pointer(TWinControl(Sender).Handle);
  Assert(p = nil, 'Trace:WARNING: [TgtkObject.SetLabel] --> got nil pointer');
  Assert(False, 'Trace:Setting Str1 in SetLabel');
  pLabel := pchar(Data);

  case TControl(Sender).fCompStyle of
  csBitBtn      : IntSendMessage3(LM_IMAGECHANGED,Sender,nil);

  csButton,
  csToolButton  :
    with PgtkButton(P)^ do
    begin
      //aLabel := StrAlloc(Length(AnsiString(PLabel)) + 1);
      aLabel := Ampersands2Underscore(PLabel);
      Try
        //StrPCopy(aLabel, AnsiString(PLabel));
        //Accel := Ampersands2Underscore(aLabel);
        if Child = nil then
        begin
          Assert(False, Format('trace:  [TgtkObject.SetLabel] %s has no child label', [Sender.ClassName]));
          child := gtk_label_new(aLabel)
        end
        else begin
          Assert(False, Format('trace:  [TgtkObject.SetLabel] %s has child label', [Sender.ClassName]));
          gtk_label_set_text(pgtkLabel(Child), aLabel);
        end;
        //If Accel <> -1 then
        AccelKey:=gtk_label_parse_uline(PGtkLabel(Child), aLabel);
        Accelerate(TComponent(Sender),PGtkWidget(P),AccelKey,0,'clicked');
      Finally
        StrDispose(aLabel);
      end;
    end;

  csForm, 
  csFileDialog, 
  csColorDialog,
  csFontDialog  : gtk_window_set_title(pGtkWindow(p),PLabel);

  csLabel:
    begin
      if TLabel(Sender).ShowAccelChar then begin
        If TLabel(sender).WordWrap then begin
          DC := GetDC(TLabel(Sender).Handle);
          aLabel := ForceLineBreaks(DC, pLabel, TLabel(Sender).Width, True);
          DeleteDC(DC);
        end
        else
          aLabel:= Ampersands2Underscore(pLabel);
        try
          AccelKey:= gtk_label_parse_uline(pGtkLabel(p), aLabel);
          Accelerate(TComponent(Sender),PGtkWidget(p),AccelKey,0,'grab_focus');
        finally
	  StrDispose(aLabel);
	end;
      end else begin
        If TLabel(sender).WordWrap then begin
          DC := GetDC(TLabel(Sender).Handle);
          aLabel := ForceLineBreaks(DC, pLabel, TLabel(Sender).Width, False);
          gtk_label_set_text(PGtkLabel(p), aLabel);
	  StrDispose(aLabel);
          DeleteDC(DC);
        end
        else
          gtk_label_set_text(PGtkLabel(p), pLabel);
        gtk_label_set_pattern(PGtkLabel(p), nil);
      end;
    end;

  csCheckBox    :
    begin
      aLabel := Ampersands2Underscore(PLabel);
      Try
        gtk_label_set_text(
                    pGtkLabel(pgtkCheckButton(p)^.Toggle_Button.Button.Child),
                    aLabel);
        gtk_label_parse_uline(pGtkLabel(pgtkCheckButton(p)^.Toggle_Button.Button.Child),
          aLabel);
      Finally
        StrDispose(aLabel);
      end;
    end;

  csGroupBox    : gtk_frame_set_label(pgtkFrame(P),pLabel);

  csEdit        : begin
                    gtk_entry_set_text(pGtkEntry(P), pLabel);
                  end;    

  csMemo        : begin
                    P:= GetWidgetInfo(P, True)^.ImplementationWidget;
                    
                    gtk_text_freeze(PGtkText(P));
                    gtk_text_set_point(PGtkText(P), 0);
                    gtk_text_forward_delete(PGtkText(P), gtk_text_get_length(PGtkText(P)));
                    gtk_text_insert(PGtkText(P), nil, nil, nil, pLabel, -1);
                    gtk_text_thaw(PGtkText(P));
                  end;      

  csPage:
    SetNotebookPageTabLabel;
    
  csComboBox    :
    begin
      //writeln('SetLabel: ',TComboBox(Sender).Name,':',TComboBox(Sender).ClassName,
      //  ' ',HexStr(Cardinal(TComboBox(Sender).Handle),8),' "',PLabel,'"');
      SetComboBoxText(PGtkCombo(TComboBox(Sender).Handle), PLabel);
    end;

  else
    Assert(True, Format ('WARNING: [TgtkObject.SetLabel] --> not handled for class %s ', [Sender.ClassName]));
  end;
  Assert(False, Format('trace:  [TgtkObject.SetLabel] %s --> END', [Sender.ClassName]));
end;

{------------------------------------------------------------------------------}
{       TGtkObject SetColor                                                 }
{       *Note: Changes the form's default background color                                 }
{------------------------------------------------------------------------------}
procedure TgtkObject.SetColor(Sender : TObject);
var
  RCStyle : PGtkRCStyle;
  Widget, FixWidget : PGTKWidget;
begin
  exit;
  {$IfNdef Win32}
  if Sender is TWinControl
  then with TWinControl(Sender) do
  begin
    // Temphack to set backcolor, till better solution
    if HandleAllocated and ((Color and SYS_COLOR_BASE)=0) then begin
      Widget:=PGtkWidget(Handle);
      
      // don't set background for custom controls (to prevent flickering)
      if GtkWidgetIsA(Widget,GTKAPIWidget_GetType) then exit;
      
      FixWidget:=GetFixedWidget(Widget);

      If FixWidget <> Widget then Widget := FixWidget;

      RCStyle:=gtk_rc_style_new;
      RCStyle^.bg[GTK_STATE_NORMAL]:=TColortoTGDKColor(Color);

      // Indicate which colors the GtkRcStyle will affect;
      // unflagged colors will follow the theme
      RCStyle^.color_flags[GTK_STATE_NORMAL]:=
        RCStyle^.color_flags[GTK_STATE_NORMAL] or GTK_RC_BG;

      gtk_widget_modify_style(Widget,RCStyle);
      gtk_rc_style_unref(RCStyle);

      //SetBKColor(Handle, ColorToRGB(Color));
    end;
  end;
  {$Else}
    Writeln('WARNING: [TgtkObject.SetColor] NOT Support under Win32 GTK')
  {$EndIf}
end;

{------------------------------------------------------------------------------
  Function: TGTKObject.SetCallback
  Params: Msg - message for which to set a callback
          sender - object to which callback will be send
  Returns:  nothing

  Applies a Message to the sender
 ------------------------------------------------------------------------------}
procedure TGTKObject.SetCallback(Msg : LongInt; Sender: TObject);

  procedure ConnectSenderSignal(const AnObject:gtk_Object; const ASignal: PChar;
    const ACallBackProc: Pointer);
  begin
    ConnectSignal(AnObject,ASignal,ACallBackProc,TComponent(Sender));
  end;

  procedure ConnectSenderSignalAfter(const AnObject:gtk_Object;
    const ASignal: PChar; const ACallBackProc: Pointer);
  begin
    ConnectSignalAfter(AnObject,ASignal,ACallBackProc,TComponent(Sender));
  end;

  procedure ConnectSenderSignal(const AnObject:gtk_Object; const ASignal: PChar;
    const ACallBackProc: Pointer; const ReqSignalMask: TGdkEventMask);
  begin
    ConnectSignal(AnObject,ASignal,ACallBackProc,TComponent(Sender),
                  ReqSignalMask);
  end;

  procedure ConnectSenderSignalAfter(const AnObject:gtk_Object;
    const ASignal: PChar; const ACallBackProc: Pointer;
    const ReqSignalMask: TGdkEventMask);
  begin
    ConnectSignalAfter(AnObject,ASignal,ACallBackProc,TComponent(Sender),
                       ReqSignalMask);
  end;

var
  gObject, gFixed, gCore, Scroll: PGTKObject;
begin
  gObject := ObjectToGTKObject(Sender);
  if gObject = nil then Exit;
  
  // gFixed is the widget with the client area (e.g. TGroupBox, TForm have this)
  gFixed := PGTKObject(GetFixedWidget(gObject));
  if gFixed = nil then gFixed := gObject;

  // gCore is the main widget (e.g. TListView has this)
  gCore:= PGtkObject(GetWidgetInfo(gObject, True)^.ImplementationWidget);

  case Msg of
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
      if (Sender is TCustomForm) then begin
        ConnectSenderSignal(gObject, 'focus-in-event', @gtkfrmactivate);
        ConnectSenderSignal(gObject, 'focus-out-event', @gtkfrmdeactivate);
      end else if Sender is TMemo then
        ConnectSenderSignal(gCore, 'activate', @gtkactivateCB)
      else
        ConnectSenderSignal(gObject, 'activate', @gtkactivateCB);
    end;

    LM_ACTIVATEITEM :
    begin
      ConnectSenderSignal(gObject, 'activate-item', @gtkactivateCB);
    end;

    LM_CHANGED :
       if Sender is TTrackBar then
       begin
         ConnectSenderSignal(gtk_Object(
                   gtk_range_get_adjustment(GTK_RANGE(gObject))) ,
                      'value_changed', @gtkvaluechanged);
       end
       else
       if Sender is TNotebook then
         ConnectSenderSignal(gObject, 'switch-page', @gtkswitchpage)
       else
       if Sender is TCustomCombobox then
         ConnectSenderSignal (PGtkObject(
                   PGtkCombo(gobject)^.entry), 'changed', @gtkchangedCB)
       else 
       if Sender is TCustomMemo then
         ConnectSenderSignal(gCore, 'changed', @gtkchanged_editbox)
       else
         ConnectSenderSignal(gObject, 'changed', @gtkchanged_editbox);

    LM_CLICKED :
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
      ConnectSenderSignalAfter(gFixed, 'draw', @GTKDrawAfter);
      ConnectSenderSignal(gFixed,'style-set', @GTKStyleChanged);
    end;

    LM_FOCUS :
    begin
      if (sender is TCustomComboBox) then begin
        ConnectSenderSignal(PgtkObject(PgtkCombo(gObject)^.entry),
                            'focus-in-event', @gtkFocusCB);
        ConnectSenderSignal(PgtkObject(PgtkCombo(gObject)^.entry),
                            'focus-out-event', @gtkKillFocusCB);
        ConnectSenderSignal(PgtkObject(PgtkCombo(gObject)^.list),
                            'focus-in-event', @gtkFocusCB);
        ConnectSenderSignal(PgtkObject(PgtkCombo(gObject)^.list),
                            'focus-out-event', @gtkKillFocusCB);
      end else begin
        ConnectSenderSignal(gCore, 'focus-in-event', @gtkFocusCB);
        ConnectSenderSignal(gCore, 'focus-out-event', @gtkKillFocusCB);
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
      if (Sender is TComboBox) then begin
        ConnectSenderSignal(PgtkObject(PgtkCombo(gObject)^.entry),
          'key-press-event', @GTKKeyUpDown, GDK_KEY_PRESS_MASK);
        ConnectSenderSignal(PgtkObject(PgtkCombo(gObject)^.entry),
          'key-release-event', @GTKKeyUpDown, GDK_KEY_RELEASE_MASK);
      end
      else if (Sender is TCustomForm) then begin
        ConnectSenderSignal(gObject, 'key-press-event', @GTKKeyUpDown,
                            GDK_KEY_PRESS_MASK);
        ConnectSenderSignal(gObject, 'key-release-event', @GTKKeyUpDown,
                            GDK_KEY_RELEASE_MASK);
      end
      else
      if (Sender is TCustomListBox) then Begin
      
        //TODO:listbox is STILL not sendig keypress events even with these lines.

        ConnectSenderSignal(gCore, 'key-press-event', @GTKKeyUpDown,
                            GDK_KEY_PRESS_MASK);
        ConnectSenderSignal(gCore, 'key-release-event', @GTKKeyUpDown,
                            GDK_KEY_RELEASE_MASK);
      end;
      ConnectSenderSignal(gCore, 'key-press-event', @GTKKeyUpDown,
                          GDK_KEY_PRESS_MASK);
      ConnectSenderSignal(gCore, 'key-release-event', @GTKKeyUpDown,
                          GDK_KEY_RELEASE_MASK);
    end;
    
    LM_MONTHCHANGED :  //calendar
    Begin
      ConnectSenderSignal(gObject, 'month-changed', @gtkmonthchanged);
      ConnectSenderSignal(gObject, 'prev-month', @gtkmonthchanged);
      ConnectSenderSignal(gObject, 'next-month', @gtkmonthchanged);
    end;

    LM_PRESSED :
    begin
      Assert(False, 'Trace:OBSOLETE: [TGTKObject.SetCallback] LM_PRESSED');
      ConnectSenderSignal(gObject, 'pressed', @gtkpressedCB);
    end;

    LM_RELEASED :
    begin
      Assert(False, 'Trace:OBSOLETE: [TGTKObject.SetCallback] LM_RELEASED');
      ConnectSenderSignal(gObject, 'released', @gtkreleasedCB);
    end;

    LM_MOVECURSOR :
    begin
      ConnectSenderSignal(gFixed, 'move-cursor', @gtkmovecursorCB);
    end;

    LM_MOUSEMOVE:
    begin
      if (sender is TComboBox) then
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
      if (sender is TCustomComboBox) then
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
      if (sender is TCustomComboBox) then
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
      if Sender is TButton then
        ConnectSenderSignal(gObject, 'enter', @gtkenterCB)
      else
        ConnectSenderSignal(gObject, 'focus-in-event', @gtkFocusInNotifyCB); //TODO: check this focus in is mapped to focus
    end;

    LM_EXIT :
    begin
      if Sender is TButton then
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
      if (sender is TCustomMemo) then
        ConnectSenderSignal(gCore, 'cut-clipboard', @gtkcuttoclip)
      else
        ConnectSenderSignal(gObject, 'cut-clipboard', @gtkcuttoclip);
    end;

    LM_COPYTOCLIP :
    begin
      if (sender is TCustomMemo) then
        ConnectSenderSignal(gCore, 'copy-clipboard', @gtkcopytoclip)
      else
        ConnectSenderSignal(gObject, 'copy-clipboard', @gtkcopytoclip);
    end;

    LM_PASTEFROMCLIP :
    begin
      if (sender is TCustomMemo) then
        ConnectSenderSignal(gCore, 'paste-clipboard', @gtkpastefromclip)
      else
        ConnectSenderSignal(gObject, 'paste-clipboard', @gtkpastefromclip);
    end;

    LM_HSCROLL:
    begin
      //if Sender is TCustomListView
      //then begin
      //  ConnectSenderSignal(gObject, 'scroll-horizontal', @gtkLVHScroll);
      //end
      //else begin
      If Sender is TScrollBox then begin
        Scroll := gtk_object_get_data(gObject, 'scroll_area');
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
      //if Sender is TCustomListView
      //then begin
      //  ConnectSenderSignal(gObject, 'scroll-vertical', @gtkLVVScroll);
      //end
      //else begin
      If Sender is TScrollBox then begin
        Scroll := gtk_object_get_data(gObject, 'scroll_area');
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
      if Sender is TComboBox then begin
        ConnectSenderSignalAfter(PgtkObject(PgtkCombo(gObject)^.popwin),
          'show', @gtkComboBoxShowCB);
        ConnectSenderSignalAfter(PgtkObject(PgtkCombo(gObject)^.popwin),
          'hide', @gtkComboBoxHideCB);
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
    Assert(False, Format('Trace:ERROR:  Signal %d not found!', [Msg]));
  end;
end;


{------------------------------------------------------------------------------
  Function: TGTKObject.RemoveCallBacks
  Params:   sender - object for which to remove callbacks
  Returns:  nothing

  Removes Call Back Signals from the sender
 ------------------------------------------------------------------------------}
procedure TGTKObject.RemoveCallbacks(Sender : TObject);
var
  MainWidget, ClientWidget, ImplWidget: PGtkWidget;
  WinWidgetInfo: PWinWidgetInfo;
begin
  MainWidget := PGtkWidget(ObjectToGTKObject(Sender));
  if MainWidget = nil then Exit;
  if (Sender is TMenuItem) then exit;

  ClientWidget:=GetFixedWidget(MainWidget);
  WinWidgetInfo:=GetWidgetInfo(MainWidget,false);
  if WinWidgetInfo<>nil then
    ImplWidget:=WinWidgetInfo^.ImplementationWidget
  else
    ImplWidget:=nil;

  gtk_signal_handlers_destroy(PGtkObject(MainWidget));
  if (ClientWidget<>nil) and (ClientWidget<>MainWidget) then
    gtk_signal_handlers_destroy(PGtkObject(ClientWidget));
  if (ImplWidget<>nil)
  and (ImplWidget<>ClientWidget) and (ImplWidget<>MainWidget) then
    gtk_signal_handlers_destroy(PGtkObject(ImplWidget));
end;

{-------------------------------------------------------------------------------
  TgtkObject.DestroyLCLControl
  Params: Sender: TObject

  Destroy the widget and all associated data
-------------------------------------------------------------------------------}
procedure TGTKObject.DestroyLCLControl(Sender : TObject);
var
  handle: hwnd; // handle of sender
  QueueItem, OldQueueItem: PLazQueueItem;
  MsgPtr: PMsg;
  Widget: PGtkWidget;
  FixWidget: PGtkWidget;
  GtkWindow: PGtkWidget;
  Accelerators: PGSlist;
  AccelEntry : PGtkAccelEntry;
begin
  Handle := hwnd(ObjectToGtkObject(Sender));
  if Handle=0 then exit;
  Widget:=PGtkWidget(Handle);
  
  FixWidget:=GetFixedWidget(Widget);
  SetWidgetIsDestroyingHandle(Widget);

  // if one of its widgets has the focus then unfocus
  GtkWindow:=gtk_widget_get_toplevel(Widget);
  if GtkWidgetIsA(GtkWindow,gtk_window_type)
  and (GetParentLCLObject(PGtkWindow(GtkWindow)^.Focus_Widget)=Sender)
  then begin
    gtk_window_set_focus(PGtkWindow(GtkWindow),nil);
  end;

  // Remove control accelerators - has to be done due to GTK+ bug?
  if Sender is TWinControl then begin
    Accelerators:= gtk_accel_group_entries_from_object(PGtkObject(Handle));
    while Accelerators <> nil do begin
      AccelEntry:= Accelerators^.data;
      Accelerators:= Accelerators^.next;
      with AccelEntry^ do
        gtk_accel_group_remove(accel_group, accelerator_key, accelerator_mods,
                               PGtkObject(Handle));
    end;
  end;
  ClearAccelKey(Widget);

  RemoveCallbacks(Sender);

  if Sender is TControl then begin
    case TControl(Sender).fCompStyle of
    csComboBox:
      begin
        SetComboBoxText(PGtkCombo(Handle),nil);
        FreeWinWidgetInfo(PGtkCombo(Handle)^.Entry);
        FreeWinWidgetInfo(PGtkCombo(Handle)^.Button);
      end;

    end;
  end
  else if Sender is TCommonDialog then begin
    DestroyCommonDialogAddOns(TCommonDialog(Sender));
  end;

  // remove pending size messages
  UnsetResizeRequest(Widget);
  FWidgetsResized.Remove(Widget);
  if FixWidget<>Widget then
    FFixWidgetsResized.Remove(FixWidget);

  //writeln('>>> LM_DESTROY ',Sender.Classname,' Sender=',HexStr(Cardinal(Sender),8),' Handle=',HexStr(Cardinal(Handle),8));
  // update mouse capturing
  if (MouseCaptureWidget=Widget) or (MouseCaptureWidget=FixWidget) then
    MouseCaptureWidget:=nil;
  
  // update clipboard widget
  if (ClipboardWidget=Widget) or (ClipboardWidget=FixWidget) then
  begin
    // clipboard widget destroyed
    if (Application<>nil) and (Application.MainForm<>nil)
    and (Application.MainForm.HandleAllocated)
    and (Application.MainForm.Handle<>Handle) then
      // there is still the main form left -> use it for clipboard
      SetClipboardWidget(PGtkWidget(Application.MainForm.Handle))
    else
      // program closed -> close clipboard
      SetClipboardWidget(nil);
  end;
  
  // update caret
  if gtk_type_is_a(gtk_object_type(PGtkObject(Handle)),GTKAPIWidget_GetType)
  then
    DestroyCaret(Handle);
    
  // destroy the widget
  DestroyWidget(Widget);

  //writeln('>>> LM_DESTROY END ',Sender.Classname,' Sender=',HexStr(Cardinal(Sender),8),' Handle=',HexStr(Cardinal(Handle),8));
  // remove all remaining messages to this component
  QueueItem:=FMessageQueue.First;
  while (QueueItem<>nil) do begin
    MsgPtr := PMsg(QueueItem^.Data);
    if (MsgPtr^.hWnd=Handle) then begin
      // remove message
      if (MsgPtr^.Message=LM_PAINT) or (MsgPtr^.Message=LM_GtkPAINT) then begin
        FPaintMessages.Remove(QueueItem);
        if MsgPtr^.Message=LM_PAINT then
          ReleaseDC(0,MsgPtr^.WParam);
      end;
      Dispose(MsgPtr);
      OldQueueItem:=QueueItem;
      QueueItem:=QueueItem^.Next;
      FMessageQueue.Delete(OldQueueItem);
    end else begin
      QueueItem:=QueueItem^.Next;
    end;
  end;

  // mouse click messages
  if LastLeft.Component=Sender then
    LastLeft:=EmptyLastMouseClick;
  if LastMiddle.Component=Sender then
    LastMiddle:=EmptyLastMouseClick;
  if LastRight.Component=Sender then
    LastRight:=EmptyLastMouseClick;
end;

{-------------------------------------------------------------------------------
  TgtkObject.HookSignals
  Params: Sender: TObject
  
  Set default Callbacks
-------------------------------------------------------------------------------}
procedure TgtkObject.HookSignals(Sender: TObject);
begin
  if (Sender is TWinControl) then
     Begin
       SetCallback(LM_SHOWWINDOW,Sender);
       SetCallback(LM_DESTROY,Sender);
       SetCallback(LM_FOCUS,Sender);
       SetCallback(LM_WINDOWPOSCHANGED,Sender);
       SetCallback(LM_PAINT,Sender);
       SetCallback(LM_EXPOSEEVENT,Sender);
       SetCallback(LM_KEYDOWN,Sender);
       SetCallback(LM_KEYUP,Sender);
       SetCallback(LM_CHAR,Sender);
       SetCallback(LM_MOUSEMOVE,Sender);
       SetCallback(LM_LBUTTONDOWN,Sender);
       SetCallback(LM_LBUTTONUP,Sender);
       SetCallback(LM_RBUTTONDOWN,Sender);
       SetCallback(LM_RBUTTONUP,Sender);
       SetCallback(LM_MBUTTONDOWN,Sender);
       SetCallback(LM_MBUTTONUP,Sender);
       SetCallback(LM_MOUSEWHEEL,Sender);
     End;

  if (Sender is TControl) then
    Begin
      case (sender as TControl).FCompStyle of

        csButton,csBitBtn:
        Begin
          SetCallback(LM_CLICKED,Sender);
        End;

        csCalendar:
        Begin
          SetCallback(LM_MONTHCHANGED,Sender);
          SetCallback(LM_YEARCHANGED,Sender);
          SetCallback(LM_DAYCHANGED,Sender);
        End;

        csComboBox:
        Begin
          SetCallback(LM_CHANGED,Sender);
          SetCallback(LM_COMMAND,Sender);
        End;

        csNotebook,csTrackBar :
        Begin
          SetCallback(LM_CHANGED,Sender);
        End;

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
	
        csFixed :
        begin
          SetCallback(LM_HSCROLL,Sender);
          SetCallback(LM_VSCROLL,Sender);
        end;

        csForm:
        Begin
          SetCallback(LM_CONFIGUREEVENT,Sender);
          SetCallback(LM_CLOSEQUERY,Sender);
          SetCallBack(LM_Activate,Sender);
          //SetCallback(LM_MOUSEENTER,Sender);
          //SetCallback(LM_MOUSELEAVE,Sender);
        end;
        
        csLabel:
        Begin
          SetCallback(LM_GRABFOCUS,Sender);
        end;

        csListview:
        begin
          SetCallback(LM_HSCROLL,Sender);
          SetCallback(LM_VSCROLL,Sender);
          SetCallback(LVN_COLUMNCLICK,Sender); 
          SetCallback(LVN_ITEMCHANGED,Sender);
          SetCallback(LVN_ITEMCHANGING,Sender);
          SetCallback(LVN_DELETEITEM,Sender);
          SetCallback(LVN_INSERTITEM,Sender);
        end;

        csScrollBox :
        Begin
          SetCallback(LM_HSCROLL,Sender);
          SetCallback(LM_VSCROLL,Sender);
        end;

      end; //case
    end
    else
    If (Sender is TMenuItem) then
      Begin
        SetCallback(LM_ACTIVATE,Sender);
      end;

end;

{------------------------------------------------------------------------------
  procedure InitializeCommonDialog
  Params: ADialog: TCommonDialog; AWindow: PGtkWidget
  Result: none

  Initializes a TCommonDialog window.
------------------------------------------------------------------------------}
procedure InitializeCommonDialog(ADialog: TObject;
  AWindow: PGtkWidget);
var NewWidth, NewHeight: integer;
begin
  SetLCLObject(AWindow,ADialog);

  // connect events
  gtk_signal_connect(gtk_object(AWindow),
    'destroy', gtk_Signal_Func(@gtkDialogDestroyCB), ADialog);
  gtk_signal_connect(gtk_object(AWindow),
    'delete-event', gtk_Signal_Func(@gtkDialogCloseQueryCB), ADialog);
  gtk_signal_connect(gtk_object(AWindow),
    'key-press-event', gtk_Signal_Func(@GTKDialogKeyUpDownCB), ADialog);
  gtk_signal_connect(gtk_object(AWindow),
    'key-release-event', gtk_Signal_Func(@GTKDialogKeyUpDownCB), ADialog);
  gtk_signal_connect(gtk_object(AWindow),
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
    LabelWidget:=gtk_label_new('History:');
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
      SetLCLObject(MenuItemWidget,OpenDialog);
      // connect the new MenuItem to the HistoryList entry
      gtk_object_set_data(PGtkObject(MenuItemWidget), 'LCLIsHistoryMenuItem',
        HistoryList[i]);
      // add activation signal and add to menu
      gtk_signal_connect(GTK_OBJECT(MenuItemWidget), 'activate',
                         gtk_signal_func(@GTKDialogMenuActivateCB),
                         OpenDialog);
      gtk_menu_append(GTK_MENU(MenuWidget), MenuItemWidget);
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
    AddEntries(CurDesc,CurMultiMask);
    // next filter
    CurDescStart:=CurMultiMaskEnd+1;
  end;
  Masks.Free;
end;

{------------------------------------------------------------------------------
  Function: CreateOpenDialogFilter
  Params: OpenDialog: TOpenDialog; SelWidget: PGtkWidget
  Returns: -

  Adds a Filter pulldown to a gtk file selection dialog.
 ------------------------------------------------------------------------------}
procedure CreateOpenDialogFilter(OpenDialog: TOpenDialog;
  SelWidget: PGtkWidget);
var
  FilterList: TList;
  HBox, LabelWidget, FilterPullDownWidget,
  MenuWidget, MenuItemWidget: PGtkWidget;
  i, CurMask: integer;
begin
  ExtractFilterList(OpenDialog.Filter,FilterList);
  if FilterList.Count>0 then begin

    // create a HBox so that the filter pulldown is left justified
    HBox:=gtk_hbox_new(false,0);
    gtk_object_set_data(PGtkObject(SelWidget), 'LCLFilterHBox', HBox);
    gtk_box_pack_start(GTK_BOX(GTK_FILE_SELECTION(SelWidget)^.main_vbox),
                       HBox,false,false,0);

    // create the label 'Filter:'
    LabelWidget:=gtk_label_new('Filter:');
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
      SetLCLObject(MenuItemWidget,OpenDialog);
      // connect the new MenuItem to the FilterList entry
      gtk_object_set_data(PGtkObject(MenuItemWidget), 'LCLIsFilterMenuItem',
        FilterList[i]);
      // add activation signal and add to menu
      gtk_signal_connect(GTK_OBJECT(MenuItemWidget), 'activate',
                         gtk_signal_func(@GTKDialogMenuActivateCB),
                         OpenDialog);
      gtk_menu_append(GTK_MENU(MenuWidget), MenuItemWidget);
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
  Function: InitializeOpenDialog
  Params: OpenDialog: TOpenDialog; SelWidget: PGtkWidget
  Returns: -

  Adds some functionality to a gtk file selection dialog.
  - multiselection
  - range selection
  - close on escape
  - file information
  - history pulldown
  - filter pulldown
 ------------------------------------------------------------------------------}
procedure InitializeOpenDialog(OpenDialog: TOpenDialog;
  SelWidget: PGtkWidget);
var
  FileDetailLabel, HBox, Frame: PGtkWidget;
begin
  // Multiselection
  if ofAllowMultiSelect in OpenDialog.Options then
  begin
    LastFileSelectRow := -1;
    gtk_signal_connect(gtk_object(PGtkCList(
      PGtkFileSelection(SelWidget)^.file_list)),
      'select-row',
      gtk_signal_func(@gtkOpenDialogRowSelectCB), OpenDialog);
    gtk_clist_set_selection_mode(
      PGtkCList(PGtkFileSelection(SelWidget)^.file_list),
      GTK_SELECTION_MULTIPLE);
  end;

  // Help button
  if (ofShowHelp in OpenDialog.Options)
  and (GTK_FILE_SELECTION(SelWidget)^.Help_Button<>nil) then begin
    gtk_widget_show(GTK_FILE_SELECTION(SelWidget)^.Help_Button);
    gtk_signal_connect( gtk_object((PGtkFileSelection(SelWidget))^.help_button),
      'clicked', gtk_signal_func(@gtkDialogHelpclickedCB), OpenDialog);
  end;

  // connect selection entry (edit field for filename)
  if (GTK_FILE_SELECTION(SelWidget)^.selection_entry<>nil) then begin
    SetLCLObject(GTK_FILE_SELECTION(SelWidget)^.selection_entry,OpenDialog);
    gtk_signal_connect(
      gtk_object((PGtkFileSelection(SelWidget))^.selection_entry),
      'key-press-event', gtk_signal_func(@GTKDialogKeyUpDownCB),
      OpenDialog);
    gtk_signal_connect(
      gtk_object((PGtkFileSelection(SelWidget))^.selection_entry),
      'focus-in-event', gtk_signal_func(@GTKDialogFocusInCB), OpenDialog);
  end;

  // connect dir list (list of directories)
  if (GTK_FILE_SELECTION(SelWidget)^.dir_list<>nil) then begin
    SetLCLObject(GTK_FILE_SELECTION(SelWidget)^.dir_list,OpenDialog);
    gtk_signal_connect(gtk_object((PGtkFileSelection(SelWidget))^.dir_list),
      'select-row', gtk_signal_func(@GTKDialogSelectRowCB), OpenDialog);
  end;

  // connect file list (list of files in current directory)
  if (GTK_FILE_SELECTION(SelWidget)^.file_list<>nil) then begin
    SetLCLObject(GTK_FILE_SELECTION(SelWidget)^.file_list,OpenDialog);
    gtk_signal_connect(gtk_object((PGtkFileSelection(SelWidget))^.file_list),
      'select-row', gtk_signal_func(@GTKDialogSelectRowCB), OpenDialog);
  end;

  // History List - a frame with an option menu
  CreateOpenDialogHistory(OpenDialog,SelWidget);

  // Filter - a frame with an option menu
  CreateOpenDialogFilter(OpenDialog,SelWidget);

  // Details - a frame with a label
  if (ofViewDetail in OpenDialog.Options) then begin
    // create the frame around the information
    Frame:=gtk_frame_new('File information');
    gtk_box_pack_start(GTK_BOX(GTK_FILE_SELECTION(SelWidget)^.main_vbox),
                       Frame,false,false,0);
    gtk_widget_show(Frame);
    // create a HBox, so that the information is left justified
    HBox:=gtk_hbox_new(false,0);
    gtk_container_add(GTK_CONTAINER(Frame), HBox);
    // create the label for the file information
    FileDetailLabel:=gtk_label_new('permissions user group size date time');
    gtk_box_pack_start(GTK_BOX(HBox),FileDetailLabel,false,false,5);
    gtk_widget_show_all(HBox);
  end else
    FileDetailLabel:=nil;
  gtk_object_set_data(PGtkObject(SelWidget), 'FileDetailLabel',
                      FileDetailLabel);

  // set initial filename
  if OpenDialog.Filename<>'' then
    gtk_file_selection_set_filename(GTK_FILE_SELECTION(SelWidget),
      PChar(OpenDialog.Filename));
end;

{------------------------------------------------------------------------------
  Function: InitializeFileDialog
  Params: FileDialog: TFileDialog; var SelWidget: PGtkWidget
  Returns: -

  Creates a new TFile/Open/SaveDialog
 ------------------------------------------------------------------------------}
procedure InitializeFileDialog(FileDialog: TFileDialog;
  var SelWidget: PGtkWidget; Title: PChar);
begin
  SelWidget := gtk_file_selection_new(Title);

  {****This is a major hack put by Cliff Baeseman to solve
   a gtk win32 dll implementation problem where the headers implementation
   does not match the linux version**** }
  {$IFNDEF WIN32}
    gtk_signal_connect(gtk_object(PGtkFileSelection(SelWidget)^.ok_button),
      'clicked', gtk_signal_func(@gtkDialogOKclickedCB), FileDialog);
    gtk_signal_connect(gtk_object(PGtkFileSelection(SelWidget)^.cancel_button),
      'clicked', gtk_signal_func(@gtkDialogCancelclickedCB), FileDialog);
  {$ELSE}
    gtk_signal_connect(gtk_object(PGtkFileSelection(SelWidget)^.cancel_button),
      'clicked', gtk_signal_func(@gtkDialogOKclickedCB), FileDialog);
    gtk_signal_connect(gtk_object(PGtkFileSelection(SelWidget)^.help_button),
      'clicked', gtk_signal_func(@gtkDialogCancelclickedCB), FileDialog);
  {$ENDIF}

  if FileDialog is TOpenDialog then
    InitializeOpenDialog(TOpenDialog(FileDialog),SelWidget);

  InitializeCommonDialog(TCommonDialog(FileDialog),SelWidget);
end;

{------------------------------------------------------------------------------
  Function: InitializeFontDialog
  Params: FontDialog: TFontialog; var SelWidget: PGtkWidget
  Returns: -

  Creates a new TFontDialog
 ------------------------------------------------------------------------------}
procedure InitializeFontDialog(FontDialog: TFontDialog;
  var SelWidget: PGtkWidget; Title: PChar);
begin
  SelWidget := gtk_font_selection_dialog_new(Title);

  // connect Ok, Cancel and Apply Button
  gtk_signal_connect(
    gtk_object(PGtkFontSelectionDialog(SelWidget)^.ok_button),
    'clicked', gtk_signal_func(@gtkDialogOKclickedCB), FontDialog);
  gtk_signal_connect(
    gtk_object(PGtkFontSelectionDialog(SelWidget)^.cancel_button),
    'clicked', gtk_signal_func(@gtkDialogCancelclickedCB), FontDialog);
  gtk_signal_connect(
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
  Function: TGTKObject.CreateComboBox
  Params:   ComboBox: TComboBox
  Returns:  PGtkCombo
  
  
-------------------------------------------------------------------------------}
function CreateComboBox(ComboBox: TComboBox): Pointer;
var
  Widget: PGtkCombo;
  ItemList: TGtkListStringList;
begin
  Result:= gtk_combo_new();
  Widget:= PGTKCombo(Result);
  
  SetMainWidget(Result, Widget^.entry);
  
  gtk_combo_disable_activate(Widget);
  gtk_combo_set_case_sensitive(Widget, 1);
  
  // Items
  ItemList:= TGtkListStringList.Create(PGtkList(Widget^.List),ComboBox);
  gtk_object_set_data(PGtkObject(Widget), 'LCLList', ItemList);
  ItemList.Assign(ComboBox.Items);
  ItemList.Sorted:= ComboBox.Sorted;

  // ItemIndex
  if ComboBox.ItemIndex >= 0 then
    gtk_list_select_item(PGtkList(Widget^.list), ComboBox.ItemIndex);

  // MaxLength
  gtk_entry_set_max_length(PGtkEntry(Widget^.entry), ComboBox.MaxLength);

  // Text
  SetComboBoxText(Widget, PChar(ComboBox.Text));
end;

Procedure TgtkObject.FinishComponentCreate(Sender : TObject;
  Handle : Pointer; SetupProps : Boolean);
begin
  // MWE: next will be obsoleted by WinWidgetInfo
  //Set these for functions like GetWindowLong  Added 01/07/2000
  if Handle <> nil then
  Begin
    SetLCLObject(Handle, Sender);
    gtk_object_set_data(pgtkObject(Handle),'Style',0);
    gtk_object_set_data(pgtkObject(Handle),'ExStyle',0);
  end;
  //--------------------------

  if (Sender is TWinControl) then
  begin
    TWinControl(Sender).Handle := THandle(Handle);
    if Handle <> nil then begin
      gtk_object_set_data(pgtkobject(Handle),'Sender',Sender);
      SetResizeRequest(Handle);
    end;
  end
  else
  if (Sender is TMenuItem) then
    TMenuItem(Sender).Handle := HMenu(Handle)
  else
  if (Sender is TMenu) then
    TMenu(Sender).Items.Handle := HMenu(Handle)
  else
  if (Sender is TCommonDialog) then
    TCommonDialog(Sender).Handle:= THandle(Handle);


  Set_RC_Name(Sender, Handle);

  if SetupProps then SetProperties(Sender);

  if Handle <> nil then begin
    {$IfNDef win32}
    if Sender is TCustomForm then
      gtk_widget_set_app_paintable(Handle,true);
    {$EndIf}
    HookSignals(Sender);
  end;
end;

Function TgtkObject.GetCompStyle(Sender : TObject) : Longint;
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

Function TgtkObject.GetCaption(Sender : TObject) : String;
begin
  Result := Sender.ClassName;
  if (Sender is TControl) then
    Result := TControl(Sender).Caption
  else
    if (Sender is TMenuItem) then
      Result := TMenuItem(Sender).Caption;

  if Result = '' then Result := 'Blank';
end;

function TgtkObject.CreateAPIWidget(
  AWinControl: TWinControl): PGtkWidget;
var
  Adjustment: PGTKAdjustment;    // currently only used for csFixed
  WinWidgetInfo: PWinWidgetInfo;
begin
  Result := GTKAPIWidget_New;
  WinWidgetInfo:=GetWidgetInfo(Result,true);
  WinWidgetInfo^.ImplementationWidget:=PGTKAPIWidget(Result)^.Client;
  SetLCLObject(WinWidgetInfo^.ImplementationWidget,AWinControl);

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

{------------------------------------------------------------------------------
  Function: TGTKObject.CreateComponent
  Params:   sender - object for which to create visual representation
  Returns:  nothing

  Tells GTK Engine to create a widget
 ------------------------------------------------------------------------------}
procedure TgtkObject.CreateComponent(Sender : TObject);
var
  Caption : ansistring;          // the caption of "Sender"
  StrTemp : PChar;               // same as "caption" but as PChar
  TempWidget,
  TempWidget2 : PGTKWidget;       // pointer to gtk-widget (local use when neccessary)
  p          : pointer;          // ptr to the newly created GtkWidget
  CompStyle,                     // componentstyle (type) of GtkWidget which will be created
  TempInt   : Integer;           // local use when neccessary
  // - for csBitBtn
  Box       : Pointer;           // currently only used for TBitBtn and TForm and TListView
  pixmapwid : pGtkWidget;        // currently only used for TBitBtn
  label1    : pgtkwidget;        // currently only used for TBitBtn
  ParentForm: TCustomForm;
  AccelText : PChar;
  AccelKey  : guint;
  SetupProps : boolean;
  Titles: PPGChar;
  ABorderStyle: TFormBorderStyle;
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

  csBitBtn :
    begin
      p := gtk_button_new;
      if ((Sender as TBitBtn).Layout in [blGlyphLeft, blGlyphRight]) then
        Box := gtk_hbox_new(False,0)
      else
        Box := gtk_vbox_new(False,0);

      gtk_container_set_border_width(PgtkContainer(Box),2);

      PixMapWid := nil;
      Label1 := gtk_label_new(StrTemp);

      gtk_box_pack_start(pGTkBox(Box), Label1, FALSE, FALSE, 3);
      gtk_widget_show(Label1);
      gtk_Container_add(PgtkContainer(p),Box);
      gtk_widget_show(Box);

      gtk_object_set_data(pgtkObject(p),'HBox',Box);
      gtk_object_set_data(pgtkObject(p),'Pixmap',PixMapwid);
      gtk_object_set_data(pgtkObject(p),'Label',Label1);
    end;

  csButton :
    begin
      AccelText := Ampersands2Underscore(StrTemp);
      p := gtk_button_new_with_label(AccelText);
      AccelKey:=gtk_label_parse_uline(PGtkLabel(PGTKButton(P)^.Child), AccelText);
      Accelerate(TComponent(Sender),PGtkWidget(p),AccelKey,0,'clicked');
      StrDispose(AccelText);
    end;

  csCalendar :
    begin
      p := gtk_calendar_new();
    end;

  csCheckbox :
    begin
      p := gtk_check_button_new_with_label(strTemp);
    end;

  csClistBox :
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
      GetWidgetInfo(p, True)^.ImplementationWidget := TempWidget;
    end;

  csColorDialog :
    begin
      P := gtk_color_selection_dialog_new(StrTemp);
      // We will only add this line if we see problem in the future with the
      // color dialog
      // gtk_color_selection_set_update_policy(GTK_COLOR_SELECTION(
      //   GTK_COLOR_SELECTION_DIALOG(P)^.colorsel), GTK_UPDATE_DISCONTINUOUS);
      gtk_signal_connect( gtk_object((GTK_COLOR_SELECTION_DIALOG(P))^.ok_button),
        'clicked', gtk_signal_func(@gtkDialogOKclickedCB), Sender);
      gtk_signal_connect( gtk_object((GTK_COLOR_SELECTION_DIALOG(P))^.cancel_button),
        'clicked', gtk_signal_func(@gtkDialogCancelclickedCB), Sender);
      InitializeCommonDialog(TCommonDialog(Sender),p);
    end;

  csComboBox :
    p:=CreateComboBox(TComboBox(Sender));

  csEdit :
    p :=  gtk_entry_new();

  csFileDialog :
    InitializeFileDialog(TFileDialog(Sender),p,StrTemp);

  csFontDialog :
    InitializeFontDialog(TFontDialog(Sender),p,StrTemp);

  csFixed: // used for TWinControl (and TCustomControl),
           // maybe change this to csWinControl
    p:=CreateAPIWidget(TWinControl(Sender));

  csForm :
    begin
      if csDesigning in TCustomForm(Sender).ComponentState then
        ABorderStyle:=bsSizeable
      else
        ABorderStyle:=TCustomForm(Sender).BorderStyle;
      
      p := gtk_window_new(FormStyleMap[ABorderStyle]);

      gtk_window_set_policy(GTK_WINDOW(p), FormResizableMap[ABorderStyle],
        FormResizableMap[ABorderStyle], 0);
      gtk_window_set_title(pGtkWindow(p), strTemp);

      // the clipboard needs a widget
      if ClipboardWidget=nil then
        SetClipboardWidget(p);

      Box := CreateFormContents(P);
      gtk_container_add(p, Box);
      gtk_widget_show(Box);

      //drag icons
      if Drag_Icon = nil then
        Drag_Icon := gdk_pixmap_colormap_create_from_xpm_d (nil,
                             gtk_widget_get_colormap (p), @Drag_Mask,
                             nil, @IMGDrag_Icon);

      // main menu
      if (TCustomForm(Sender).Menu<>nil)
      and (TCustomForm(Sender).Menu.HandleAllocated) then begin
        gtk_box_pack_start(Box, PGtkWidget(TCustomForm(Sender).Menu.Handle),
                           False, False, 0);
      end;
    end;

  csFrame :
    begin
      P := gtk_frame_new(' ');
      gtk_frame_set_shadow_type(pGtkFrame(P),GTK_SHADOW_NONE);
    end;

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

  csGTKTable  :
    begin
       P := gtk_table_new(2,2,False);
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
       gdk_window_set_decorations(AWindow,
         GetWindowDecorations(TCustomForm(Sender)));
       gdk_window_set_functions(AWindow,
         GetWindowFunction(TCustomForm(Sender)));
       gtk_widget_show_all(p);
    end;

  csImage :
    Begin
      p := gtk_image_new(nil,nil);
    end;


  csLabel :
    begin
      P := gtk_label_new(StrTemp);
      SetupProps:= true;
    end;

  csListBox :
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
      GetWidgetInfo(p, True)^.ImplementationWidget := TempWidget;
    end;

  csListView :
    Begin
      p:= gtk_scrolled_window_new(nil, nil);

      with TListView(Sender)
      do begin
        TempInt:=Columns.Count;
        if TempInt<1 then TempInt:=1;
        TempWidget:= gtk_clist_new(TempInt);
        gtk_clist_set_shadow_type(PGtkCList(TempWidget),GTK_SHADOW_IN);
        gtk_clist_column_titles_passive (GTK_CLIST (TempWidget));

        // add items (the item properties are set via LM_SETPROPERTIES)
        GetMem(Titles,SizeOf(PGChar)*GTK_CLIST (TempWidget)^.columns);
        for TempInt:=0 to GTK_CLIST (TempWidget)^.columns-1 do
          Titles[TempInt]:=nil;
        for TempInt:=0 to Items.Count-1 do begin
          if Items[TempInt].Caption<>'' then
            Titles[0] := PChar(Items[TempInt].Caption)
          else
            Titles[0] := #0;
          gtk_clist_append(PgtkCList(TempWidget),Titles);
        end;
        FreeMem(Titles);

        // set columns properties
        for TempInt := 0 to Columns.Count - 1 do begin
          with Columns[TempInt] do begin
            // set title
            gtk_clist_set_column_title(PgtkCList(TempWidget),TempInt,
              PChar(Caption));
            //set column alignment
            gtk_clist_set_column_justification(PgtkCList(TempWidget),TempInt,
                     aGTKJUSTIFICATION[Alignment]);
            //set width
            gtk_clist_set_column_width(PgtkCList(TempWidget),TempInt,Width);
            //set auto sizing
            gtk_clist_set_column_auto_resize(PgtkCList(TempWidget),TempInt,
                     AutoSize);
            //set Visible
            gtk_clist_set_column_visibility(PgtkCList(TempWidget),TempInt,
                     Visible);
            // set MinWidth
            if MinWidth>0 then
              gtk_clist_set_column_min_width(PGtkCList(TempWidget), TempInt,
                                             MinWidth);
            // set MaxWidth
            if (MaxWidth>=MinWidth) and (MaxWidth>0) then
              gtk_clist_set_column_max_width(PGtkCList(TempWidget), TempInt,
                                             MaxWidth);
          end;
        end;

      end;
      gtk_clist_column_titles_passive (GTK_CLIST (TempWidget));

      gtk_container_add(GTK_CONTAINER(p),TempWidget);
      GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.hscrollbar, GTK_CAN_FOCUS);
      GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.vscrollbar, GTK_CAN_FOCUS);
      gtk_scrolled_window_set_policy(PGtkScrolledWindow(p),
                                     GTK_POLICY_AUTOMATIC,
                                     GTK_POLICY_AUTOMATIC);
      gtk_container_set_focus_vadjustment(PGtkContainer(TempWidget),
                    gtk_scrolled_window_get_vadjustment(PGtkScrolledWindow(p)));
      gtk_container_set_focus_hadjustment(PGtkContainer(TempWidget),
                    gtk_scrolled_window_get_hadjustment(PGtkScrolledWindow(p)));
      gtk_widget_show_all(TempWidget);
      gtk_widget_show(p);

      SetMainWidget(p, TempWidget);
      GetWidgetInfo(p, True)^.ImplementationWidget := TempWidget;
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
      GetWidgetInfo(p, True)^.ImplementationWidget := TempWidget;

      gtk_widget_show_all(TempWidget);
      gtk_widget_show_all(P);

      SetupProps:= true;
    end;

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

  csPage:    // TPage - Notebook page
    begin
      // create a fixed widget in a horizontal box
      P := gtk_hbox_new(false, 0);
      TempWidget := gtk_fixed_new();
      gtk_container_add(GTK_CONTAINER(P), TempWidget);
      gtk_widget_show(TempWidget);
      SetFixedWidget(p, TempWidget);
      SetMainWidget(p, TempWidget);
      gtk_widget_show(P);
    end;

  csPanel:
    with (TPanel(Sender)) do
    begin
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

  csProgressBar:
    with (TProgressBar (Sender)) do
    begin
       { Create a GtkAdjustment object to hold the range of the progress bar }
       TempWidget := PGtkWidget( gtk_adjustment_new (Position, Min, Max, 0, 0, 0));
       { Create the GtkProgressBar using the adjustment }
       P := gtk_progress_bar_new_with_adjustment (PGtkAdjustment (TempWidget));
    end;

  csRadioButton :
    with TRadioButton(Sender) do
    begin
      { Look for our parent's control and use the first radio we find for grouping }
      TempWidget:= nil;
      if Parent <> nil then begin
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
                                   pGtkLabel(PGTKToggleButton(P)^.Button.Child),
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
       P := gtk_statusbar_new();
    end;

  csToggleBox :
    begin
       P := gtk_toggle_button_new_with_label(StrTemp);
    end;

  csToolbar:
    begin
       p := gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL,GTK_TOOLBAR_BOTH);
       gtk_widget_show (P);
    end;

  csToolButton:
    begin
      AccelText := Ampersands2Underscore(StrTemp);
      //p := gtk_button_new_with_label(StrTemp);
      p := gtk_button_new_with_label(AccelText);
      if TToolButton(Sender).Style = tbsButton then
      Begin
        //If Accel <> -1 then
        AccelKey:=gtk_label_parse_uline(PGtkLabel(PGTKButton(P)^.Child),
                                        AccelText);
        Accelerate(TComponent(Sender),PGtkWidget(P),AccelKey,0,'clicked');
      end;
      StrDispose(AccelText);
      gtk_widget_show (P);
    end;

  csTrackBar:
    with (TTrackBar (Sender)) do
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

      gtk_object_set_data(P,'scroll_area', TempWidget);
      
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
end;

{------------------------------------------------------------------------------}
{       TGtkObject AssignSelf                                                  }
{       *Note: Assigns a pointer to self on a widget                           }
{------------------------------------------------------------------------------}
procedure TgtkObject.AssignSelf(Child,Data : Pointer);
begin
   gtk_Object_Set_Data(Pgtkobject(Child),'Self',Data);
end;

{------------------------------------------------------------------------------}
{       TGtkObject ShowHide                                                    }
{       *Note: Show or hide a widget                                           }
{------------------------------------------------------------------------------}
procedure TgtkObject.ShowHide(Sender : TObject);

  procedure RaiseWrongClass;
  begin
    RaiseException('TgtkObject.ShowHide Sender.ClassName='+Sender.ClassName);
  end;

var FormIconGdiObject: PGDIObject;
  SenderWidget, ParentFixed, ParentWidget: PGTKWidget;
  LCLControl: TWinControl;
  Decor, Func : Longint;
  AWindow: PGdkWindow;
begin
  if not (Sender is TWinControl) then
    RaiseWrongClass;

  LCLControl:=TWinControl(Sender);
  if not LCLControl.HandleAllocated then exit;
  SenderWidget:=PgtkWidget(LCLControl.Handle);
  //if Sender is TForm then
  //  writeln('[TgtkObject.ShowHide] START ',TControl(Sender).Name,':',Sender.ClassName,' Visible=',TControl(Sender).Visible,' GtkVisible=',gtk_widget_visible(SenderWidget));
  if LCLControl.Visible
  or ((csDesigning in LCLControl.ComponentState)
    and not (csNoDesignVisible in LCLControl.ControlStyle)) then
  begin
    if (Sender is TCustomForm) then begin
      // update shared accelerators
      ShareWindowAccelGroups(SenderWidget);
    end;

    if gtk_widget_visible(SenderWidget) then
      exit;
      
    // before making the widget visible, set the position and size
    if FWidgetsWithResizeRequest.Contains(SenderWidget) then begin
      if (LCLControl is TCustomForm) and (LCLControl.Parent=nil) then begin
        // top level control (a form without parent)
        {$IFDEF VerboseFormPositioning}
        writeln('VFP [TgtkObject.ShowHide] A set bounds ',
          LCLControl.Name,':',LCLControl.ClassName,
          ' Window=',GetControlWindow(SenderWidget)<>nil,
          ' ',TControl(Sender).Left,',',TControl(Sender).Top,
          ',',TControl(Sender).Width,',',TControl(Sender).Height);
        {$ENDIF}
        SetWindowSizeAndPosition(PgtkWindow(SenderWidget),TWinControl(Sender));
      end else if (LCLControl.Parent<>nil) then begin
        // resize widget
        gtk_widget_set_usize(SenderWidget,LCLControl.Width,LCLControl.Height);
        // move widget on the fixed widget of parent control
        ParentWidget:=pgtkWidget(LCLControl.Parent.Handle);
        ParentFixed := GetFixedWidget(ParentWidget);
        if GtkWidgetIsA(ParentFixed,GTK_FIXED_GET_TYPE)
        or GtkWidgetIsA(ParentFixed,GTK_LAYOUT_GET_TYPE) then begin
          FixedMoveControl(ParentFixed, SenderWidget,
                         LCLControl.Left, LCLControl.Top);
        end else if not (LCLControl.Parent is TNoteBook) then begin
          writeln('WARNING: TgtkObject.ShowHide - no Fixed Widget found');
          writeln('  Control=',LCLControl.Name,':',LCLControl.ClassName);
        end;
      end;
      UnsetResizeRequest(SenderWidget);
    end;

    if (Sender is TCustomForm) then begin
      If (TCustomForm(Sender).BorderStyle <> bsSizeable) or
        (TCustomForm(Sender).FormStyle = fsStayOnTop)
      then begin
        Decor := GetWindowDecorations(TCustomForm(Sender));
        Func := GetWindowFunction(TCustomForm(Sender));
        gtk_widget_realize(SenderWidget);
        AWindow:=GetControlWindow(SenderWidget);
        gdk_window_set_decorations(AWindow, decor);
        gdk_window_set_functions(AWindow, func);
      end;
      ShareWindowAccelGroups(SenderWidget);
    end;
    
    gtk_widget_show(SenderWidget);
    
    SetColor(Sender);
    if (Sender is TCustomForm) then begin
      AWindow:=GetControlWindow(SenderWidget);
      if (AWindow<>nil) then begin
        FormIconGdiObject:=PGDIObject(TCustomForm(Sender).GetIconHandle);
        if (FormIconGdiObject<>nil) then begin
          gdk_window_set_icon(AWindow, nil,
                  FormIconGdiObject^.GDIBitmapObject,
                  FormIconGdiObject^.GDIBitmapMaskObject);
        end;
      end;
    end;
  end
  else Begin
  
    if (Sender is TCustomForm) then begin
      UnshareWindowAccelGroups(SenderWidget);
    end;
  
    gtk_widget_hide(SenderWidget);
  end;
  //if Sender is TForm then
  //  writeln('[TgtkObject.ShowHide] END ',Sender.ClassName,' Window=',FormWidget^.Window<>nil);
end;

{-------------------------------------------------------------------------------
  method TGtkObject LoadXPMFromLazResource
  Params: const ResourceName: string;
          Window: PGdkWindow;
          var PixmapImg, PixmapMask: PGdkPixmap
  Result: none

  Loads a pixmap from a lazarus resource. The resource must be a XPM file.
-------------------------------------------------------------------------------}
procedure TGtkObject.LoadXPMFromLazResource(const ResourceName: string;
  Window: PGdkWindow; var PixmapImg, PixmapMask: PGdkPixmap);
var
  ImgData: PPGChar;
begin
  PixmapImg:=nil;
  PixmapMask:=nil;
  try
    ImgData:=PPGChar(LazResourceXPMToPPChar(ResourceName));
  except
    on e: Exception do
      writeln('WARNING: TGtkObject.LoadXPMFromLazResource: '+e.Message);
  end;
  PixmapImg:=gdk_pixmap_create_from_xpm_d(Window,@PixmapMask,nil,ImgData);
  FreeMem(ImgData);
end;

{-------------------------------------------------------------------------------
  method TGtkObject GetNoteBookCloseBtnPixmap
  Params: ANoteBook: TCustomNotebook; APage: TPage
  Result: none

  Loads the pixmap for the close button in the tabs of the TNoteBook(s).
-------------------------------------------------------------------------------}
procedure TgtkObject.GetNoteBookCloseBtnPixmap(Window: PGdkWindow;
  var Img, Mask: PGdkPixmap);
begin
  if (FNoteBookCloseBtnPixmapImg=nil)
  and (Window<>nil) then begin
    LoadXPMFromLazResource('tnotebook_close_tab',Window,
                        FNoteBookCloseBtnPixmapImg,FNoteBookCloseBtnPixmapMask);
  end;
  Img:=FNoteBookCloseBtnPixmapImg;
  Mask:=FNoteBookCloseBtnPixmapMask;
end;

{-------------------------------------------------------------------------------
  function GetGtkNoteBookPageCount(ANoteBookWidget: PGtkNoteBook): integer;
  
  Returns the number of pages in a PGtkNotebook
-------------------------------------------------------------------------------}
function GetGtkNoteBookPageCount(ANoteBookWidget: PGtkNoteBook): integer;
var
  AListItem: PGList;
begin
  Result:=0;
  if ANoteBookWidget=nil then exit;
  AListItem:=ANoteBookWidget^.children;
  while AListItem<>nil do begin
    inc(Result);
    AListItem:=AListItem^.Next;
  end;
end;

{-------------------------------------------------------------------------------
  procedure AddDummyNoteBookPage(NoteBookWidget: PGtkNoteBook);

  Adds the dummy page.
  A gtk notebook must have at least one page, but TNoteBook also allows no pages
  at all. Therefore at least a dummy page is added. This dummy page is removed
  as soon as other pages are added.
-------------------------------------------------------------------------------}
procedure TgtkObject.AddDummyNoteBookPage(NoteBookWidget: PGtkNoteBook);
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
    gtk_notebook_prepend_page_menu(NoteBookWidget,DummyWidget,ALabel,MenuLabel);
    SetGtkNoteBookDummyPage(NoteBookWidget,DummyWidget);
  end;
end;

{-------------------------------------------------------------------------------
  procedure RemoveDummyNoteBookPage(NoteBookWidget: PGtkNotebook);
  
  Removes the dummy page.
  See also AddDummyNoteBookPage
-------------------------------------------------------------------------------}
procedure TGtkObject.RemoveDummyNoteBookPage(NoteBookWidget: PGtkNotebook);
var
  DummyWidget: PGtkWidget;
begin
  DummyWidget:=GetGtkNoteBookDummyPage(NoteBookWidget);
  if DummyWidget=nil then exit;
  gtk_notebook_remove_page(NoteBookWidget,0);
  DummyWidget:=nil;
  SetGtkNoteBookDummyPage(NoteBookWidget,DummyWidget);
end;

{-------------------------------------------------------------------------------
  method TGtkObject UpdateNotebookPageTab
  Params: ANoteBook: TCustomNotebook; APage: TPage
  Result: none

  Updates the tab of a page of a notebook. This contains the image to the left
  side, the label, the close button, the menu image and the menu label.
-------------------------------------------------------------------------------}
procedure TgtkObject.UpdateNotebookPageTab(ANoteBook, APage: TObject);
var
  TheNoteBook: TCustomNotebook;
  ThePage: TPage;

  NoteBookWidget: PGtkWidget;  // the notebook
  PageWidget: PGtkWidget;      // the page (content widget)
  TabWidget: PGtkWidget;       // the tab (hbox containing a pixmap, a label
                               //          and a close button)
  TabPixmapWidget: PGtkWidget; // the pixmap in the tab
  TabLabelWidget: PGtkWidget;  // the label in the tab
  TabCloseBtnWidget: PGtkWidget;// the close button in the tab
  TabCloseBtnPixmapWidget: PGtkWidget; // the pixmap in the close button
  MenuWidget: PGtkWidget;      // the popup menu (hbox containing a pixmap and
                               // a label)
  MenuPixmapWidget: PGtkWidget;// the pixmap in the popup menu item
  MenuLabelWidget: PGtkWidget; // the label in the popup menu item

  procedure UpdateTabPixmap;
  var
    Img: PGdkPixmap;
    Mask: PGdkBitmap;
  begin
    Img:=nil;
    Mask:=nil;
    if (TheNoteBook.Images<>nil)
    and (ThePage.ImageIndex>=0)
    and (ThePage.ImageIndex<TheNoteBook.Images.Count) then begin
      // page has valid image

      // ToDo: get Img and Mask

    end;

    if Img<>nil then begin
      // page has an image
      if TabPixmapWidget<>nil then begin
        // there is already a pixmap for the image in the tab
        // -> replace the image
        gtk_pixmap_set(PGtkPixmap(TabPixmapWidget),Img,Mask);
      end else begin
        // there is no pixmap for the image in the tab
        // -> insert one ot the left side of the label
        TabPixmapWidget:=gtk_pixmap_new(Img,Mask);
        gtk_object_set_data(PGtkObject(TabWidget),'TabPixmap',TabPixmapWidget);
        gtk_widget_show(TabPixmapWidget);
        gtk_box_pack_start_defaults(PGtkBox(TabWidget),TabPixmapWidget);
        gtk_box_reorder_child(PGtkBox(TabWidget),TabPixmapWidget,0);
      end;
      if MenuPixmapWidget<>nil then begin
        // there is already a pixmap for the image in the menu
        // -> replace the image
        gtk_pixmap_set(PGtkPixmap(MenuPixmapWidget),Img,Mask);
      end else begin
        // there is no pixmap for the image in the menu
        // -> insert one at the left side of the label
        MenuPixmapWidget:=gtk_pixmap_new(Img,Mask);
        gtk_object_set_data(PGtkObject(MenuWidget),'TabPixmap',MenuPixmapWidget);
        gtk_widget_show(MenuPixmapWidget);
        gtk_box_pack_start_defaults(PGtkBox(MenuWidget),MenuPixmapWidget);
        gtk_box_reorder_child(PGtkBox(MenuWidget),MenuPixmapWidget,0);
      end;
    end else begin
      // page does not have an image
      if TabPixmapWidget<>nil then begin
        // there is a pixmap for an old image in the tab
        // -> remove the pixmap widget
        DestroyWidget(TabPixmapWidget);
        gtk_object_set_data(PGtkObject(TabWidget), 'TabPixmap', nil);
        TabPixmapWidget:=nil;
      end;
      if MenuPixmapWidget<>nil then begin
        // there is a pixmap for an old image in the menu
        // -> remove the pixmap widget
        DestroyWidget(MenuPixmapWidget);
        gtk_object_set_data(PGtkObject(MenuWidget), 'TabPixmap', nil);
        MenuPixmapWidget:=nil;
      end;
    end;
  end;
  
  procedure UpdateTabLabel;
  var TheCaption: PChar;
  begin
    TheCaption:=PChar(ThePage.Caption);
    if TheCaption=nil then
      TheCaption:=#0;
    gtk_label_set_text(PGtkLabel(TabLabelWidget),TheCaption);
    if MenuLabelWidget<>nil then
      gtk_label_set_text(PGtkLabel(MenuLabelWidget),TheCaption);
  end;
  
  procedure UpdateTabCloseBtn;
  var
    Img, Mask: PGdkPixmap;
  begin
    GetNoteBookCloseBtnPixmap(GetControlWindow(NoteBookWidget),Img,Mask);
    if (nboShowCloseButtons in TheNotebook.Options) and (Img<>nil) then begin
      // close buttons enabled
      if TabCloseBtnWidget=nil then begin
        // there is no close button yet
        // -> add on to the right side of the label in the tab
        TabCloseBtnWidget:=gtk_button_new;
        gtk_object_set_data(PGtkObject(TabWidget), 'TabCloseBtn',
                            TabCloseBtnWidget);
        begin
          // put a pixmap into the button
          TabCloseBtnPixmapWidget:=gtk_pixmap_new(Img,Mask);
          gtk_object_set_data(PGtkObject(TabCloseBtnWidget),'TabCloseBtnPixmap',
                              TabCloseBtnPixmapWidget);
          gtk_widget_show(TabCloseBtnPixmapWidget);
          gtk_container_add(PGtkContainer(TabCloseBtnWidget),
                            TabCloseBtnPixmapWidget);
        end;
        gtk_widget_show(TabCloseBtnWidget);
        gtk_signal_connect(PGtkObject(TabCloseBtnWidget), 'clicked',
          TGTKSignalFunc(@gtkNoteBookCloseBtnClicked), APage);
        gtk_box_pack_start_defaults(PGtkBox(TabWidget),TabCloseBtnWidget);
      end;
    end else begin
      // close buttons disabled
      if TabCloseBtnWidget<>nil then begin
        // there is a close button
        // -> remove it
        gtk_object_set_data(PGtkObject(TabWidget), 'TabCloseBtn',
                            nil);
        DestroyWidget(TabCloseBtnWidget);
        TabCloseBtnWidget:=nil;
      end;
    end;
  end;
  
begin
  ThePage:=TPage(APage);
  TheNoteBook:=TCustomNotebook(ANoteBook);
  if (APage=nil) or (not ThePage.HandleAllocated) then exit;
  if TheNoteBook=nil then begin
    TheNoteBook:=TCustomNotebook(ThePage.Parent);
    if TheNoteBook=nil then exit;
  end;
  NoteBookWidget:=PGtkWidget(TWinControl(TheNoteBook).Handle);
  PageWidget:=PGtkWidget(TWinControl(ThePage).Handle);

  // get the tab container and the tab components: pixmap, label and closebtn
  TabWidget:=gtk_notebook_get_tab_label(PGtkNoteBook(NotebookWidget),
                                        PageWidget);
  if TabWidget<>nil then begin
    TabPixmapWidget:=gtk_object_get_data(PGtkObject(TabWidget), 'TabPixmap');
    TabLabelWidget:=gtk_object_get_data(PGtkObject(TabWidget), 'TabLabel');
    TabCloseBtnWidget:=gtk_object_get_data(PGtkObject(TabWidget),'TabCloseBtn');
  end else begin
    TabPixmapWidget:=nil;
    TabLabelWidget:=nil;
    TabCloseBtnWidget:=nil;
  end;
  
  // get the menu container and its components: pixmap and label
  MenuWidget:=gtk_notebook_get_menu_label(PGtkNoteBook(NotebookWidget),
                                          PageWidget);
  if MenuWidget<>nil then begin
    MenuPixmapWidget:=gtk_object_get_data(PGtkObject(MenuWidget), 'TabPixmap');
    MenuLabelWidget:=gtk_object_get_data(PGtkObject(MenuWidget), 'TabLabel');
  end else begin
    MenuPixmapWidget:=nil;
    MenuLabelWidget:=nil;
  end;

  UpdateTabPixmap;
  UpdateTabLabel;
  UpdateTabCloseBtn;
end;

{-------------------------------------------------------------------------------
  method TGtkObject AddNBPage
  Params: ANoteBook, APage: TObject; Index: Integer
  Result: none
  
  Inserts a new page to a notebook at position Index. The ANotebook is a
  TNoteBook, the APage one of its TPage. Both handles must already be created.
  ANoteBook Handle is a PGtkNoteBook and APage handle is a PGtkFixed.
  This procedure creates a new tab with an optional image, the page caption and
  an optional close button. The image and the caption will also be added to the
  tab popup menu.
-------------------------------------------------------------------------------}
procedure TgtkObject.AddNBPage(ANoteBook, APage: TObject; Index: Integer);
var
  NoteBookWidget: PGtkWidget;  // the notebook
  PageWidget: PGtkWidget;      // the page (content widget)
  TabWidget: PGtkWidget;       // the tab (hbox containing a pixmap, a label
                               //          and a close button)
  TabLabelWidget: PGtkWidget;  // the label in the tab
  MenuWidget: PGtkWidget;      // the popup menu (hbox containing a pixmap and
                               // a label)
  MenuLabelWidget: PGtkWidget; // the label in the popup menu item
begin
  NoteBookWidget:=PGtkWidget(TWinControl(ANoteBook).Handle);
  PageWidget:=PGtkWidget(TWinControl(APage).Handle);

  // create the tab (hbox container)
  TabWidget:=gtk_hbox_new(false,1);
  begin
    gtk_object_set_data(PGtkObject(TabWidget), 'TabPixmap', nil);
    gtk_object_set_data(PGtkObject(TabWidget), 'TabCloseBtn', nil);
    // put a label into the tab
    TabLabelWidget:=gtk_label_new('');
    gtk_object_set_data(PGtkObject(TabWidget), 'TabLabel', TabLabelWidget);
    gtk_widget_show(TabLabelWidget);
    gtk_box_pack_start_defaults(PGtkBox(TabWidget),TabLabelWidget);
  end;
  gtk_widget_show(TabWidget);

  // create popup menu
  MenuWidget:=gtk_hbox_new(false,2);
  begin
    // put a pixmap into the menu
    gtk_object_set_data(PGtkObject(MenuWidget), 'TabPixmap', nil);
    // put a label into the menu
    MenuLabelWidget:=gtk_label_new('');
    gtk_object_set_data(PGtkObject(MenuWidget), 'TabLabel', MenuLabelWidget);
    gtk_widget_show(MenuLabelWidget);
    gtk_box_pack_start_defaults(PGtkBox(MenuWidget),MenuLabelWidget);
  end;

  gtk_widget_show(MenuWidget);

  RemoveDummyNoteBookPage(PGtkNotebook(NoteBookWidget));
  gtk_notebook_insert_page_menu(GTK_NOTEBOOK(NotebookWidget),PageWidget,
    TabWidget,MenuWidget,Index);

  UpdateNotebookPageTab(TNoteBook(ANoteBook),TPage(APage));
  UpdateNoteBookClientWidget(ANoteBook);
end;

{------------------------------------------------------------------------------
       TGtkObject RemoveNBPage
       *Note: Remove Notebook Page
------------------------------------------------------------------------------}
procedure TgtkObject.RemoveNBPage(ANoteBook: TObject; Index: Integer);
var
  NoteBookWidget: PGtkNotebook;
begin
  Assert(false, 'Trace:Removing a notebook page');
  NoteBookWidget:=PGtkNotebook(TWinControl(ANoteBook).Handle);
  if GetGtkNoteBookPageCount(NoteBookWidget)>1 then begin
    gtk_notebook_remove_page(NoteBookWidget, Index);
  end else begin
    AddDummyNoteBookPage(NoteBookWidget);
    gtk_notebook_remove_page(NoteBookWidget, Index+1);
  end;
  UpdateNoteBookClientWidget(ANoteBook);
end;

{------------------------------------------------------------------------------
  procedure TgtkObject.MoveNBPage(ANoteBook, APage: TObject; NewIndex: Integer);
  
  Move a notebook page.
------------------------------------------------------------------------------}
procedure TgtkObject.MoveNBPage(ANoteBook, APage: TObject; NewIndex: Integer);
var
  NoteBookWidget: PGtkNotebook;
begin
  NoteBookWidget:=PGtkNotebook(TWinControl(ANoteBook).Handle);
  gtk_notebook_reorder_child(NoteBookWidget,
                             PGtkWidget(TWinControl(APage).Handle),NewIndex);
  UpdateNoteBookClientWidget(ANoteBook);
end;

{------------------------------------------------------------------------------}
{       TGtkObject ReDraw                                                      }
{       *Note:                                                                 }
{------------------------------------------------------------------------------}
procedure TgtkObject.ReDraw(Child : Pointer);
var
  fWindow :pGdkWindow;
  widget : PgtkWIdget;
  PixMap : pgdkPixMap;
  //gc : PGDKGc;
begin
  Assert(False, 'Trace:In AutoRedraw in GTKObject');

  Widget := GetFixedWidget(Child);
  pixmap := gtk_Object_get_data(pgtkobject(Child),'Pixmap');
  if PixMap = nil then Exit;
  fWindow := GetControlWindow(widget);
  //gc := gdk_gc_new(PgdkWindow(fWindow));

  if fWindow<>nil then begin
    gdk_draw_pixmap(fwindow,
      PGtkStyle(widget^.TheStyle)^.fg_gc[GTK_WIDGET_STATE (widget)],
      pixmap,
      0,0,
      0,0,
      pgtkwidget(widget)^.allocation.width,
      pgtkwidget(widget)^.allocation.height);
  end;
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.SetPixel
  Params:  Sender : the lcl object which called this func via SendMessage
           Data   : pointer to a TLMSetGetPixel record
  Returns: nothing

  Set the color of the specified pixel on the window?screen?object?
 ------------------------------------------------------------------------------}
procedure TgtkObject.SetPixel(Sender : TObject; Data : Pointer);
var
  aDC     : TDeviceContext;
  Image   : pGDKImage;
  Widget  : PgtkWidget;
  GDKColor: TGDKColor;
  pFixed  : PGTKFixed;
  fWindow : pGdkWindow;
begin
  aDC := TDeviceContext(TCanvas(Sender).Handle);
  if aDC = nil then exit;
  Widget := PgtkWidget(aDC.Wnd);

  Image := gtk_Object_get_data(pgtkobject(widget),'Image');
  if Image = nil then begin
    fWindow:=GetControlWindow(widget);
    if fWindow=nil then exit;
    Image := gdk_image_get(fWindow,0,0,
                            widget^.allocation.width,widget^.allocation.height);
    if Image = nil then exit;
    gtk_Object_set_data(pgtkobject(Widget),'Image',Image);
  end;

  // ToDo: free allocated gdk color

  GDKColor:=AllocGDKColor(TLMSetGetPixel(data^).PixColor);
//writeln('SetPixel: Color=',HexStr(TLMSetGetPixel(data^).PixColor,8),' GDKColor=',HexStr(GDKColor.Pixel,8));
  gdk_image_put_pixel(Image,TLMSetGetPixel(data^).X,TLMSetGetPixel(data^).Y,
                         GDKColor.Pixel);

  pFixed := GetFixedWidget(Widget);
  if pFixed <> nil then Widget:=PgtkWidget(pFixed);
  fWindow := GetControlWindow(Widget);
  if fWindow<>nil then
    gdk_draw_image(fwindow,
      PGtkStyle(widget^.TheStyle)^.fg_gc[GTK_WIDGET_STATE (Widget)], 
      Image,
      TLMSetGetPixel(Data^).X,TLMSetGetPixel(data^).Y,
      TLMSetGetPixel(Data^).X,TLMSetGetPixel(data^).Y,
      1,1);
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.GetPixel
  Params:  Sender : the lcl object which called this func via SenMessage
           Data   : pointer to a TLMSetGetPixel record
  Returns: nothing

  Get the color of the specified pixel on the window?screen?object?
 ------------------------------------------------------------------------------}
procedure TgtkObject.GetPixel(Sender : TObject; Data : Pointer);
var
  aDC    : TDeviceContext;
  Image  : pGDKImage;
  Widget : PgtkWidget;
  GDKColor: TGDKColor;
  GdkColorContext: PGdkColorContext;
  AWindow: PGdkWindow;
begin
  aDC := TDeviceContext(TCanvas(Sender).Handle);
  if aDC = nil then exit;
  Widget := PgtkWidget(aDC.Wnd);

  Image := gtk_Object_get_data(pgtkobject(Widget),'Image');
  if Image = nil then begin
    AWindow:=GetControlWindow(widget);
    if AWindow=nil then exit;
    Image := gdk_image_get(AWindow,0,0,
      widget^.allocation.width,widget^.allocation.height);
    if Image = nil then exit;
    gtk_Object_set_data(pgtkobject(Widget),'Image',Image);
  end;

  GDKColor.Pixel := gdk_image_get_pixel(Image,
                              TLMSetGetPixel(data^).X,TLMSetGetPixel(data^).Y);
  GdkColorContext:=
    gdk_color_context_new(gdk_visual_get_system,gdk_colormap_get_system);
  gdk_color_context_query_color(GdkColorContext,@GDKColor);
  gdk_color_context_free(GdkColorContext);
    
  TLMSetGetPixel(data^).PixColor := TGDKColorToTColor(GDKColor);
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.SetColorDialogColor
  Params:  ColorSelection : a gtk color selection dialog;
           Color          : the color to select
  Returns: nothing

  Set the color of the coor selection dialog
 ------------------------------------------------------------------------------}
procedure TgtkObject.SetColorDialogColor(ColorSelection: PGtkColorSelection;
  Color: TColor);
var
  SelectionColor: PGDouble;       // currently only used by TColorDialog
  colorSel : GTK_COLOR_SELECTION;
begin
  GetMem(SelectionColor,4*SizeOf(GDouble));
  try
    Color:=ColorToRGB(Color);
    SelectionColor[0]:=GDouble(Color and $ff)/255;
    SelectionColor[1]:=GDouble((Color shr 8) and $ff)/255;
    SelectionColor[2]:=GDouble((Color shr 16) and $ff)/255;
    SelectionColor[3]:=0.0;
    colorSel := GTK_COLOR_SELECTION(
      (GTK_COLOR_SELECTION_DIALOG(ColorSelection))^.colorsel);
    gtk_color_selection_set_color(colorSel,SelectionColor);
  finally
    FreeMem(SelectionColor);
  end; 
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.GetValue
  Params:  Sender : the lcl object which called this func via SenMessage
           Data   : pointer to component specific variable
  Returns: currently always 0

  Depending on the compStyle, this function will get the current value
  of a GTK object and save it in the variable referenced by 'data'.
  
  This function should be used to synchronize the state of an lcl-object
  with the corresponding GTK-object.
 ------------------------------------------------------------------------------}
function TgtkObject.GetValue (Sender : TObject; data : pointer) : integer;
type
  PCheckBoxState = ^TCheckBoxState;
var
  Handle    : Pointer;
  Year,Month,Day : Integer;  //used for csCalendar
begin
  Result := 0;     // default if nobody sets it

  if Sender is TWinControl then
    Assert(False, Format('Trace:  [TgtkObject.GetValue] %s', [Sender.ClassName]))
  else
    Assert(False, Format('Trace:WARNING: [TgtkObject.GetValue] %s --> No Decendant of TWinControl', [Sender.ClassName]));

  Handle    := Pointer(TWinControl(Sender).Handle);
  Assert (Handle = nil, 'WARNING: [TgtkObject.GetValue] --> got nil pointer (no gtkobject)');
   
  case TControl(Sender).fCompStyle of
  csTrackbar   :
    if (handle <> nil) then begin
      integer(data^) := round(gtk_range_get_adjustment(
              GTK_RANGE (handle))^.value);
    end else
      integer(data^) := 0;


  csRadiobutton,
  csCheckbox   :
    if gtk_toggle_button_get_active (PGtkToggleButton (handle))
    then PCheckBoxState(data)^ := cbChecked
    else PCheckBoxState(data)^ := cbUnChecked;
                    
  csCalendar :
    Begin
      gtk_calendar_get_date(PgtkCalendar(handle),@Year, @Month, @Day);
      //TODO:  account for local settings like date format
      //Form some reason, the month is zero based.
      TLMCalendar(data^).Date :=
        StrtoDate(Inttostr(Day)+'-'+Inttostr(Month+1)+'-'+Inttostr(Year));
    end;

  csSpinEdit :
    Begin
      Single(Data^):=gtk_spin_button_get_value_As_Float(PgtkSpinButton(Handle));
    end;

  else
    Assert (true, Format ('WARNING:[TgtkObject.GetValue] failed for %s', [Sender.ClassName]));
  end;
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.SetValue
  Params:  Sender : the lcl object which called this func via SendMessage
           Data   : pointer to component specific variable
  Returns: currently always 0

  Depending on the compStyle, this function will apply the parameter 'data'
  to the GTK object repesenting the lcl-object which called the function.

  This function should for be used in cases where the most common property
  of an object has changed (e.g. the position of a trackbar). If more than
  one property changed use the SetProperties function instead;
 ------------------------------------------------------------------------------}
function TgtkObject.SetValue (Sender : TObject; data : pointer) : integer;
var
   Handle    : Pointer;
   //used for csCalendar
   Date : TDateTime;
   Year,Month,Day : String;
   gtkcalendardisplayoptions : TGtkCalendarDisplayOptions;
   Num : Integer;
   
   ArrowType : TGTKArrowType;
   ShadowType : TGTKShadowType;
begin
  result := 0;     // default if nobody sets it

  if Sender is TWinControl
  then Assert(False, Format('Trace:  [TgtkObject.SetValue] %s', [Sender.ClassName]))
  else Assert(False, Format('Trace:WARNING: [TgtkObject.SetValue] %s --> No Decendant of TWinControl', [Sender.ClassName]));

  Handle    := Pointer(TWinControl(Sender).Handle);
//   Assert (Handle = nil, 'WARNING: [TgtkObject.SetValue] --> got nil pointer (no gtkobject)');
   
  case TControl(Sender).fCompStyle of
  csProgressBar: gtk_progress_set_value (GTK_PROGRESS (handle), integer (data^));

  csTrackbar   :
    begin
      if Handle = nil then Exit;
      gtk_range_get_adjustment (GTK_RANGE (handle))^.value := integer (data^);
      gtk_signal_emit_by_name (PGtkObject
         (gtk_range_get_adjustment (
             GTK_RANGE (handle))), 'value_changed');
    end;

  csRadiobutton,
  csCheckbox   :
    begin
      gtk_toggle_button_set_active(PGtkToggleButton(handle), (TCheckBoxState(data^) = cbChecked))
    end;
                    
  csCalendar   :
    Begin
       Date := TLMCalendar(data^).Date;
       Year := FormatDateTime('yyyy',Date);
       Month := FormatDateTime('mm',Date);
       Day := FormatDateTime('dd',Date);
       gtk_calendar_select_month(PgtkCalendar(handle),StrtoInt(Month)-1,StrToInt(Year));
       gtk_calendar_select_day(PgtkCalendar(handle),StrToInt(Day));
       
       //set display options
       Num := 0;
       if (dsShowHeadings in TLMCalendar(data^).DisplaySettings) then
          num := Num + (1 shl 0);
                
       if (dsShowDayNames in TLMCalendar(data^).DisplaySettings) then
          num := Num  + (1 shl 1);

       if (dsNoMonthChange in TLMCalendar(data^).DisplaySettings) then
          num := Num  + (1 shl 2);
          
       if (dsShowWeekNumbers in TLMCalendar(data^).DisplaySettings) then
           num := Num  + (1 shl 3);

       if (dsStartMonday in TLMCalendar(data^).DisplaySettings) then
           num := Num  + (1 shl 4);

       gtkCalendarDisplayOptions := TgtkCalendarDisplayOPtions(num);
       
       gtk_Calendar_Display_options(PgtkCalendar(handle),gtkCalendarDisplayOptions);
       
       //readonly
       if TLMCalendar(data^).ReadOnly then
          gtk_calendar_freeze(PgtkCalendar(handle))
       else
          gtk_calendar_thaw(PgtkCalendar(handle));

     end;
     
  csArrow :
    begin
      if TLmArrow(data^).ArrowType = atUp then
        ArrowType := GTK_ARROW_UP
      else
      if TLMArrow(data^).ArrowType = atLeft then
        ArrowType := GTK_ARROW_LEFT
      else
      if TLMArrow(data^).ArrowType = atRight then
        ArrowType := GTK_ARROW_RIGHT
      else
        ArrowType := GTK_ARROW_DOWN;

      case TLMArrow(data^).ShadowType of
        stNONE : ShadowType := GTK_SHADOW_NONE;
        stIN : ShadowType := GTK_SHADOW_IN;
        stOut : ShadowType := GTK_SHADOW_OUT;
        stEtchedIn : ShadowType := GTK_SHADOW_ETCHED_IN;
        stEtchedOut : ShadowType := GTK_SHADOW_ETCHED_OUT;
      else
        ShadowType := GTK_SHADOW_NONE;
      end;
              
      gtk_arrow_set(PgtkArrow(handle),ArrowType,ShadowType);
    end;
                 
  else
    Assert (true, Format ('WARNING:[TgtkObject.SetValue] failed for %s', [Sender.ClassName]));
  end;
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.SetProperties
  Params:  Sender : the lcl object which called this func via SenMessage
  Returns: currently always 0

  Depending on the compStyle, this function will apply all properties of
  the calling object to the corresponding GTK object.
 ------------------------------------------------------------------------------}
function TgtkObject.SetProperties(Sender : TObject) : integer;
const
  cLabelAlignX : array[TAlignment] of gfloat = (0.0, 1.0, 0.5);
  cLabelAlignY : array[TTextLayout] of gfloat = (0.0, 0.5, 1.0);
  cLabelAlign : array[TAlignment] of TGtkJustification = (GTK_JUSTIFY_LEFT, GTK_JUSTIFY_RIGHT, GTK_JUSTIFY_CENTER);
var
   Handle    : Pointer;
   Widget, ImplWidget   : PGtkWidget;
   I,X         : Integer;
   pRowText : PChar;
   BitImage : TBitMap;
begin
  Result := 0;     // default if nobody sets it

  if Sender is TWinControl
  then
    Assert(False, Format('Trace:  [TgtkObject.SetProperties] %s', [Sender.ClassName]))
  else
    RaiseException('TgtkObject.SetProperties: '
                   +' Sender.ClassName='+Sender.ClassName);

  Handle:= Pointer(TWinControl(Sender).Handle);
  Widget:= PGtkWidget(Handle);
   
  case TControl(Sender).fCompStyle of
    csEdit :
      with TCustomEdit(Sender) do
        begin
          // XXX TODO: GTK 1.x does not support EchoMode emNone.
          // This will have to be coded around, but not a priority
        
          gtk_entry_set_editable(PgtkEntry(Handle), not (TCustomEdit(Sender).ReadOnly));
          gtk_entry_set_max_length(PgtkEntry(Handle), TCustomEdit(Sender).MaxLength);
          gtk_entry_set_visibility(PGtkEntry(Handle),
            (TCustomEdit(Sender).EchoMode = emNormal)
            and (TCustomEdit(Sender).PassWordChar=#0));
        end;

    csProgressBar :
       with TProgressBar(Sender) do
       begin
          Widget := PGtkWidget( gtk_adjustment_new (0, Min, Max, 0, 0, 0));
          gtk_progress_set_adjustment (GTK_PROGRESS (handle), PGtkAdjustment (Widget));
          gtk_progress_set_value (GTK_PROGRESS (handle), Position);
          if Smooth 
             then gtk_progress_bar_set_bar_style (GTK_PROGRESS_BAR (handle), GTK_PROGRESS_CONTINUOUS)
             else gtk_progress_bar_set_bar_style (GTK_PROGRESS_BAR (handle), GTK_PROGRESS_DISCRETE);
          case Orientation of
             pbVertical      : gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR (handle), GTK_PROGRESS_BOTTOM_TO_TOP);
             pbRightToLeft   : gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR (handle), GTK_PROGRESS_RIGHT_TO_LEFT);
             pbTopDown       : gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR (handle), GTK_PROGRESS_TOP_TO_BOTTOM);
          else { pbHorizontal is default }
             gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR (handle), GTK_PROGRESS_LEFT_TO_RIGHT);
          end;
          if BarShowText then 
          begin
             gtk_progress_set_format_string (GTK_PROGRESS (handle), '%v from [%l-%u] (=%p%%)');
             gtk_progress_set_show_text (GTK_PROGRESS (handle), 1);
          end
          else
             gtk_progress_set_show_text (GTK_PROGRESS (handle), 0);
       end;


    csScrollBar:
       with (TScrollBar (Sender)) do
           begin
             //set properties for the range
             Widget := PGtkWidget (gtk_range_get_adjustment (GTK_RANGE (handle)));
             PGtkAdjustment(Widget)^.lower := Min;
             PGtkAdjustment(Widget)^.Upper := Max;
             PGtkAdjustment(Widget)^.Value := Position;
             PGtkAdjustment(Widget)^.step_increment := SmallChange;
             PGtkAdjustment(Widget)^.page_increment := LargeChange;
           end;
 
    csTrackbar   : 
       with (TTrackBar (Sender)) do
       begin
          Widget := PGtkWidget (gtk_range_get_adjustment (GTK_RANGE (handle)));
          PGtkAdjustment(Widget)^.lower := Min;
          PGtkAdjustment(Widget)^.Upper := Max;
          PGtkAdjustment(Widget)^.Value := Position;
          PGtkAdjustment(Widget)^.step_increment := LineSize; 
          PGtkAdjustment(Widget)^.page_increment := PageSize;
          { now do some of the more sophisticated features }
          { Hint: For some unknown reason we have to disable the draw_value first,
            otherwise it's set always to true }
          gtk_scale_set_draw_value (PGTKSCALE (handle), false);

          if ShowScale then
          begin
             gtk_scale_set_draw_value (PGTKSCALE (handle), ShowScale);
             case ScalePos of
                trLeft  : gtk_scale_set_value_pos (PGTKSCALE (handle), GTK_POS_LEFT);
                trRight : gtk_scale_set_value_pos (PGTKSCALE (handle), GTK_POS_RIGHT);
                trTop   : gtk_scale_set_value_pos (PGTKSCALE (handle), GTK_POS_TOP);
                trBottom: gtk_scale_set_value_pos (PGTKSCALE (handle), GTK_POS_BOTTOM);
             end;
          end;
          //Not here (Delphi compatibility):  gtk_signal_emit_by_name (GTK_Object (Widget), 'value_changed');
       end;

    csLabel :
       with TLabel(Sender) do
       begin
         gtk_label_set_justify(PGtkLabel(Handle), cLabelAlign[Alignment]);
         gtk_misc_set_alignment(PGtkMisc(Handle), cLabelAlignX[Alignment],
                                cLabelAlignY[Layout]);
         gtk_label_set_line_wrap(PGtkLabel(Handle), WordWrap);
       end;
       
    csListView :
      begin
        // set up columns..

        Widget:= GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget;
        
        gtk_clist_freeze(PgtkCList(Widget));
        
        
        for I := 0 to TListview(sender).Columns.Count-1 do
        begin
          gtk_clist_set_column_title(Pgtkclist(Widget),I,
                                 PChar(TListview(sender).Columns[i].Caption));

          // set column alignment
          gtk_clist_set_column_justification(PgtkCList(Widget),I,
                   aGTKJUSTIFICATION[TListview(sender).Columns[i].Alignment]);
          // set auto sizing
          gtk_clist_set_column_auto_resize(PgtkCList(Widget),I,
                   TListview(sender).Columns[i].AutoSize);
          // set width
          gtk_clist_set_column_width(PgtkCList(Widget),I,
                   TListview(sender).Columns[i].Width);
          // set Visible
          gtk_clist_set_column_visibility(PgtkCList(Widget),I,
                   TListview(sender).Columns[i].Visible);
          // set MinWidth
          if TListview(sender).Columns[i].MinWidth>0 then
            gtk_clist_set_column_min_width(PGtkCList(Widget), I,
                                       TListview(sender).Columns[i].MinWidth);
          // set MaxWidth
          if (TListview(sender).Columns[i].MaxWidth>=
            TListview(sender).Columns[i].MinWidth)
          and (TListview(sender).Columns[i].MaxWidth>0) then
            gtk_clist_set_column_max_width(PGtkCList(Widget), I,
                                       TListview(sender).Columns[i].MaxWidth);
        end;

        //sorting
        if (TListview(sender).ViewStyle = vsReport)
        then gtk_clist_column_titles_show(PgtkCList(Widget))
        else gtk_clist_column_titles_Hide(PgtkCList(Widget));

        gtk_clist_set_sort_column(PgtkCList(Widget),
          TListview(sender).SortColumn);
              
        //multiselect
        gtk_clist_set_selection_mode(PgtkCList(Widget),
          aGTkSelectionMode[TListview(sender).MultiSelect]);

//TODO:This doesn't work right now
//                gtk_clist_set_auto_sort(PgtkCList(handle),TListview(sender).Sorted);
        //
        //do items...
        //
        for I := 0 to TListview(sender).Items.Count-1 do
        begin
          pRowText:=PChar(TListItem(TListview(sender).Items[i]).Caption);
          gtk_clist_set_text(Pgtkclist(Widget),I,0,pRowText);

          //do image if one is assigned....
          // TODO: Largeimage support
          if  (TListview(sender).SmallImages <> nil)
          and (TListItem(TListview(sender).Items[i]).ImageIndex > -1) 
          then begin
            Writeln('Checking images');
            if (TListItem(TListview(sender).Items[i]).ImageIndex
              < TListview(sender).SmallImages.Count)
            then begin
              //draw image
              Writeln('drawing image');
              Writeln('TListItem(TListview(sender).Items[i]).ImageIndex is ',TListItem(TListview(sender).Items[i]).ImageIndex);
                    
              BitImage := TBitmap.Create;
              TListview(sender).SmallImages.GetBitmap(
                 TListItem(TListview(sender).Items[i]).ImageIndex,BitImage);
              
              gtk_clist_set_pixmap(Pgtkclist(Widget),I,0,
                pgdkPixmap(PgdiObject(BitImage.handle)^.GDIBitmapObject),
                nil);
              {$IfNDef Win32}
              gtk_clist_set_pixtext(Pgtkclist(Widget),I,0,pRowText,3,
                pgdkPixmap(PgdiObject(BitImage.handle)^.GDIBitmapObject),
                nil);
              {$EndIF}
//                      bitimage.Free;
            end;
          end;

          if (TListview(sender).ViewStyle = vsReport) 
          then begin //columns showing
            for X := 1 to TListview(sender).Columns.Count-1 do
            begin
              if ( X <= TListItem(TListview(sender).Items[i]).SubItems.Count) 
              then begin
                pRowText:=PChar(TListItem(
                  TListview(sender).Items[i]).SubItems.Strings[X-1]);
                gtk_clist_set_text(Pgtkclist(Widget),I,X,pRowText);
              end;
            end;  //for loop
          end;
        end;
        gtk_clist_thaw(PgtkCList(Widget));
      end;

    csMemo:
      begin
        ImplWidget:= GetWidgetInfo(Widget, true)^.ImplementationWidget;
      
        gtk_text_set_editable (PGtkText(ImplWidget), not (Sender as TCustomMemo).ReadOnly);
        {$IfNDef Win32}
	gtk_text_set_line_wrap(PGtkText(ImplWidget), Integer((Sender as TCustomMemo).WordWrap));
        {$EndIf}
	gtk_text_set_word_wrap(PGtkText(ImplWidget), 1);
        
        case (Sender as TCustomMemo).Scrollbars of
          ssHorizontal: gtk_scrolled_window_set_policy(PGtkScrolledWindow(Widget), GTK_POLICY_ALWAYS, GTK_POLICY_NEVER);
          ssVertical:   gtk_scrolled_window_set_policy(PGtkScrolledWindow(Widget), GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
          ssBoth:       gtk_scrolled_window_set_policy(PGtkScrolledWindow(Widget), GTK_POLICY_ALWAYS, GTK_POLICY_ALWAYS);
          ssAutoHorizontal: gtk_scrolled_window_set_policy(PGtkScrolledWindow(Widget), GTK_POLICY_AUTOMATIC, GTK_POLICY_NEVER);
          ssAutoVertical:   gtk_scrolled_window_set_policy(PGtkScrolledWindow(Widget), GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
          ssAutoBoth:       gtk_scrolled_window_set_policy(PGtkScrolledWindow(Widget), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
        else
          gtk_scrolled_window_set_policy(PGtkScrolledWindow(Widget), GTK_POLICY_NEVER, GTK_POLICY_NEVER);
        end;
	
	if (TCustomMemo(Sender).MaxLength >= 0) then begin
	  i:= gtk_text_get_length(PGtkText(ImplWidget));
	  if i > TCustomMemo(Sender).MaxLength then begin
            gtk_editable_delete_text(PGtkEditable(ImplWidget), TCustomMemo(Sender).MaxLength, i);
	  end;
	end;  
      end;
           
    csSpinEdit:
      Begin
        // ToDo: set climb_rate
        gtk_spin_button_set_digits(PgtkSpinButton(Handle),
                                   TSpinEdit(Sender).Decimal_Places);
        gtk_spin_button_set_value(PgtkSpinButton(Handle),
                                  TSpinEdit(Sender).Value);
      End;

    else
       Assert (true, Format ('WARNING:[TgtkObject.SetProperties] failed for %s', [Sender.ClassName]));
   end;
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.AttachMenu
  Params:  Sender : the lcl object which called this func 
  Returns: nothing

  Attaches the calling Menu to its Parent
 ------------------------------------------------------------------------------}
procedure TGtkObject.AttachMenu(Sender: TObject);
var
  //AccelKey: Integer;
  //AccelGroup: PGTKAccelGroup;
  MenuItem, ParentMenuWidget, ContainerMenu: PGtkWidget;
  LCLMenuItem: TMenuItem;
  
  procedure SetContainerMenuToggleSize;
  var MenuClass: PGtkWidgetClass;
  begin
    if GtkWidgetIsA(ContainerMenu,GTK_MENU_TYPE) then begin
      MenuClass:=GTK_WIDGET_CLASS(PGtkObject(ContainerMenu)^.klass);
      if OldMenuSizeRequestProc=nil then begin
        OldMenuSizeRequestProc:=MenuClass^.size_request;
      end;
      MenuClass^.size_request:=@MenuSizeRequest;
    end;
  end;
  
begin
  LCLMenuItem:=TMenuItem(Sender);
  //writeln('TGtkObject.AttachMenu START ',LCLMenuItem.Name,':',LCLMenuItem.ClassName,' Parent=',LCLMenuItem.Parent.Name,':',LCLMenuItem.Parent.ClassName);
  with LCLMenuItem do
  begin
    MenuItem := PGtkWidget(Handle);
    if MenuItem=nil then
      RaiseException('TGtkObject.AttachMenu Handle=0');
    ParentMenuWidget := PGtkWidget(Parent.Handle);
    if ParentMenuWidget=nil then
      RaiseException('TGtkObject.AttachMenu ParentMenuWidget=nil');

    if GtkWidgetIsA(ParentMenuWidget,GTK_MENU_BAR_TYPE) then begin
      // mainmenu
      ContainerMenu:=ParentMenuWidget;
      gtk_menu_bar_insert(PGtkMenuBar(ParentMenuWidget),MenuItem,
                          LCLMenuItem.MenuIndex);
    end
    else begin
      // find the menu container
      ContainerMenu := PGtkWidget(gtk_object_get_data(
                                                   PGtkObject(ParentMenuWidget),
                                                   'ContainerMenu'));
      if ContainerMenu = nil then begin
        if (GetParentMenu is TPopupMenu) and (Parent.Parent=nil) then begin
          ContainerMenu:=PGtkWidget(GetParentMenu.Handle);
          gtk_object_set_data(PGtkObject(ContainerMenu), 'ContainerMenu',
                              ContainerMenu);
        end else begin
          ContainerMenu := gtk_menu_new;
          gtk_object_set_data(PGtkObject(ParentMenuWidget), 'ContainerMenu',
                              ContainerMenu);
          gtk_menu_item_set_submenu(PGTKMenuItem(ParentMenuWidget),ContainerMenu);

          //AccelGroup := GetAccelGroup(ContainerMenu,true);
          //gtk_menu_set_accel_group(PGtkMenu(ContainerMenu), AccelGroup);
        end;
      end;
      gtk_menu_insert(PGtkMenu(ContainerMenu), MenuItem, LCLMenuItem.MenuIndex);
    end;
    
    SetContainerMenuToggleSize;
    
    if GtkWidgetIsA(MenuItem,GTK_RADIO_MENU_ITEM_TYPE) then
      RegroupMenuItem(HMENU(MenuItem),GroupIndex);

    {// Add accelerators
    AccelGroup := GetAccelGroup(ContainerMenu,false);
    AccelKey := GetAccelKey(MenuItem);
    if (AccelGroup <> nil) and (AccelKey <> 0) then begin
      gtk_accel_group_add(AccelGroup, AccelKey, GDK_MOD1_MASK,
                          GTK_ACCEL_LOCKED, PGtkObject(MenuItem),
                          'activate_item');
    end;}
  end;
  //writeln('TGtkObject.AttachMenu END ',LCLMenuItem.Name,':',LCLMenuItem.ClassName);
end;

{------------------------------------------------------------------------------
  Function: IsValidDC
  Params:  DC: a (LCL) devicecontext
  Returns: True if valid

  Checks if the given DC is valid.
 ------------------------------------------------------------------------------}
function TgtkObject.IsValidDC(const DC: HDC): Boolean;
begin
  Result := FDeviceContexts.Contains(Pointer(DC));
end;

{------------------------------------------------------------------------------
  Function: IsValidGDIObject
  Params:  GDIObject: a (LCL) gdiObject
  Returns: True if valid

  Checks if the given GDIObject is valid
 ------------------------------------------------------------------------------}
function TgtkObject.IsValidGDIObject(const GDIObject: HGDIOBJ): Boolean;
begin
  Result := (GDIObject<>0) and (FGDIObjects.Contains(Pointer(GDIObject)));
  if Result then
    with PGdiObject(GDIObject)^ do
      case GDIType of
        gdiBitmap : begin
           case GDIBitmapType of
             gbPixmap: Result := GDIPixmapObject <> nil;
             gbBitmap: Result := GDIBitmapObject <> nil;
             gbImage:  Result := GDIRawImageObject <> nil;
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
function TgtkObject.IsValidGDIObjectType(
  const GDIObject: HGDIOBJ; const GDIType: TGDIType): Boolean;
begin
  Result := IsValidGDIObject(GDIObject)
            and (PGdiObject(GDIObject)^.GDIType = GDIType);
end;

{------------------------------------------------------------------------------
  Procedure: TGTKObject.SelectGDKBrushProps
  Params:  DC: a (LCL)devicecontext
  Returns: Nothing

  Sets the forecolor and fill according to the brush
 ------------------------------------------------------------------------------}
procedure TGTKObject.SelectGDKBrushProps(DC: HDC);
begin
  if (TDeviceContext(DC).SelectedColors=dcscBrush) or
     TDeviceContext(DC).CurrentBrush^.IsNullBrush
  then 
    exit;
  
  with TDeviceContext(DC), CurrentBrush^ do
  begin
    Assert(False, Format('Trace:  [SelectGDKBrushProps] Fill: %d | Color: 0x%x', [Integer(GDIBrushFill), GDIBrushColor.ColorRef]));

    EnsureGCColor(DC, dccCurrentBackColor, True, True);//BKColor
    EnsureGCColor(DC, dccGDIBrushColor, GDIBrushFill = GDK_Solid, False);//Brush Color

    If GDIBrushFill <> GDK_Solid then
      If GDIBrushPixmap <> nil then begin
        gdk_gc_set_fill(GC, GDIBrushFill);
        gdk_error_trap_push;//Image errors can kill us
        gdk_gc_set_Stipple(GC,GDIBrushPixmap);
      end
  end;
  TDeviceContext(DC).SelectedColors:=dcscBrush;
end;

{------------------------------------------------------------------------------
  Procedure: TGTKObject.SelectGDKTextProps
  Params:  DC: a (LCL)devicecontext
  Returns: Nothing

  Sets the forecolor and fill according to the Textcolor
 ------------------------------------------------------------------------------}
procedure TGTKObject.SelectGDKTextProps(DC: HDC);
begin
  if TDeviceContext(DC).SelectedColors=dcscFont then exit;
  
  with TDeviceContext(DC) do
  begin
    EnsureGCColor(DC, dccCurrentBackColor, True, True);//BKColor
    EnsureGCColor(DC, dccCurrentTextColor, False, False);//Font Color
    Assert(False, Format('trace:  [SelectGDKTextProps] Color --> pixel: %d, red: 0x%x, green: 0x%x, blue: 0x%x', [CurrentTextColor.Color.Pixel, CurrentTextColor.Color.Red, CurrentTextColor.Color.Green, CurrentTextColor.Color.Blue]));
  end;
  TDeviceContext(DC).SelectedColors:=dcscFont;
end;

{------------------------------------------------------------------------------
  Procedure: TGTKObject.TGTKObject.SelectGDKPenProps
  Params:  DC: a (LCL)devicecontext
  Returns: Nothing

  Sets the forecolor and fill according to the pen
 ------------------------------------------------------------------------------}
procedure TGTKObject.SelectGDKPenProps(DC: HDC);

  procedure SetDashes(const Dashes: array of gint8);
  begin
    laz_gdk_gc_set_dashes(TDeviceContext(DC).GC,0,Pgint8(@Dashes[Low(Dashes)]),
      High(Dashes)-Low(Dashes)+1);
  end;

begin
  if TDeviceContext(DC).SelectedColors=dcscPen then exit;
  
  with TDeviceContext(DC), CurrentPen^ do begin
    EnsureGCColor(DC, dccCurrentBackColor, True, True);//BKColor
    EnsureGCColor(DC, dccGDIPenColor, False, False);//Pen Color
  end;

  if (not (dcfPenSelected in TDeviceContext(DC).DCFlags)) then begin
    Exclude(TDeviceContext(DC).DCFlags,dcfPenInvalid);
    if TDeviceContext(DC).GC<>nil then begin
      with TDeviceContext(DC), CurrentPen^ do
      begin
        IsNullPen := GDIPenStyle = PS_NULL;
        if (GDIPenStyle = PS_SOLID) or (GDIPenStyle = PS_INSIDEFRAME)
        then begin
          gdk_gc_set_line_attributes(GC, GDIPenWidth, GDK_LINE_SOLID, 0, 0)
        end
        else begin
          gdk_gc_set_line_attributes(GC,GDIPenWidth,GDK_LINE_ON_OFF_DASH,0,0);
          case GDIPenStyle of
            PS_DASH:       SetDashes([4,4]);
            PS_DOT:        SetDashes([2,2]);
            PS_DASHDOT:    SetDashes([4,2,2,2,0]);
            PS_DASHDOTDOT: SetDashes([4,2,2,2,2,2]);
           {This is DEADLY!!!}
            //PS_NULL:       gdk_gc_set_dashes(GC, 0, [0,4], 2);
          end;
        end;
      end;
      Include(TDeviceContext(DC).DCFlags,dcfPenSelected);
    end;
  end;
  TDeviceContext(DC).SelectedColors:=dcscPen;
end;

{------------------------------------------------------------------------------
  Function: NewDC
  Params:  none
  Returns: a gtkwinapi DeviceContext

  Creates an initial DC
 ------------------------------------------------------------------------------}
function TgtkObject.NewDC: TDeviceContext;
begin
  Assert(False, Format('Trace:> [TgtkObject.NewDC]', []));
  Result:=GtkDef.NewDeviceContext;
  with Result do
  begin
    gdk_color_black(gdk_colormap_get_system, @CurrentTextColor);
    gdk_color_white(gdk_colormap_get_system, @CurrentBackColor);
  end;
  FDeviceContexts.Add(Result);
//writeln('[TgtkObject.NewDC] ',HexStr(Cardinal(Result),8),'  ',FDeviceContexts.Count);
//  Assert(False, Format('Trace:< [TgtkObject.NewDC] FDeviceContexts[%d] --> 0x%p', [n, Result]));
end;

{------------------------------------------------------------------------------
  procedure TgtkObject.DisposeDC(DC: PDeviceContext);

  Disposes a DC
 ------------------------------------------------------------------------------}
procedure TgtkObject.DisposeDC(aDC: TDeviceContext);
begin
  if FDeviceContexts.Contains(aDC) then begin
    FDeviceContexts.Remove(aDC);
    GtkDef.DisposeDeviceContext(aDC);
  end;
end;

{------------------------------------------------------------------------------
  function TgtkObject.CreateDCForWidget(TheWidget: PGtkWidget;
    TheWindow: PGdkWindow): HDC;

  Creates an initial DC
 ------------------------------------------------------------------------------}
function TgtkObject.CreateDCForWidget(TheWidget: PGtkWidget;
  TheWindow: PGdkWindow): HDC;
  
  procedure RaiseWidgetWithoutClientArea;
  begin
    RaiseException('TgtkObject.CreateWindowDC widget '
      +HexStr(Cardinal(TheWidget),8)+' has no client area');
  end;
  
var
  aDC: TDeviceContext;
  ClientWidget: PGtkWidget;
  GdiObject: PGdiObject;
  Values: TGdkGCValues;
  X,Y : Longint;
begin
  aDC := nil;

  aDC := NewDC;
  aDC.Wnd := HWND(TheWidget);

  if TheWidget = nil
  then begin
    FillChar(Values, SizeOf(Values), #0);
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
          RaiseException('TgtkObject.CreateDCForWidget: Unable to realize GdkWindow');
      end;
    end else
      ClientWidget:=TheWidget;

    aDC.SpecialOrigin:=GtkWidgetIsA(ClientWidget,GTK_LAYOUT_GET_TYPE);
    aDC.Drawable := TheWindow;
    aDC.GC := gdk_gc_new(aDC.Drawable);
    gdk_window_get_size(aDC.Drawable, @X, @Y);
    gdk_gc_set_function(aDC.GC, GDK_COPY);

    gdk_gc_get_values(aDC.GC, @Values);
  end;

  if aDC <> nil
  then begin
    if Values.Font <> nil
    then begin
      GdiObject:=NewGDIObject(gdiFont);
      GdiObject^.GDIFontObject := Values.Font;
      gdk_font_ref(Values.Font);
    end
    else GdiObject := CreateDefaultFont;

    aDC.CurrentFont := GdiObject;
    aDC.CurrentBrush := CreateDefaultBrush;
    aDC.CurrentPen := CreateDefaultPen;
  end;


  Result := HDC(aDC);
  Assert(False, Format('trace:< [TgtkObject.GetDC] Got 0x%x', [Result]));
end;

{------------------------------------------------------------------------------
  Function: NewGDIObject
  Params:  none
  Returns: a gtkwinapi DeviceContext

  Creates an initial GDIObject of GDIType.
 ------------------------------------------------------------------------------}
function TgtkObject.NewGDIObject(const GDIType: TGDIType): PGdiObject;
begin
  Assert(False, Format('Trace:> [TgtkObject.NewGDIObject]', []));
  Result:=GtkDef.NewPGDIObject;
  FillChar(Result^,SizeOf(TGDIObject),0);
  Result^.GDIType := GDIType;
  FGDIObjects.Add(Result);
//writeln('[TgtkObject.NewGDIObject] ',HexStr(Cardinal(Result),8),'  ',FGDIObjects.Count);
  Assert(False, Format('Trace:< [TgtkObject.NewGDIObject] FGDIObjects --> 0x%p', [Result]));
end;

{------------------------------------------------------------------------------
  Function: NewGDIObject
  Params:  GdiObject: PGdiObject
  Returns: none

  Dispose a GdiObject
 ------------------------------------------------------------------------------}
procedure TgtkObject.DisposeGDIObject(GDIObject: PGdiObject);
begin
  if FGDIObjects.Contains(GDIObject) then begin
    FGDIObjects.Remove(GDIObject);
    GtkDef.DisposePGDIObject(GDIObject);
  end;
end;

{------------------------------------------------------------------------------
  Function: CreateDefaultBrush
  Params:  none
  Returns: a Brush GDIObject

  Creates an default brush, used for initial values
 ------------------------------------------------------------------------------}
function TgtkObject.CreateDefaultBrush: PGdiObject;
begin
//write('  TgtkObject.CreateDefaultBrush ->');
  Result := NewGDIObject(gdiBrush);
  Result^.GDIBrushFill := GDK_SOLID;
  Result^.GDIBrushColor.ColorRef := 0;
  Result^.GDIBrushColor.Colormap := gdk_colormap_get_system;
  gdk_color_white(Result^.GDIBrushColor.Colormap, @Result^.GDIBrushColor.Color);
  With Result^.GDIBrushColor.Color do
    Result^.GDIBrushColor.ColorRef :=
      RGB(GetGValue(Red), GetGValue(Green), GetGValue(Blue));
  Include(Result^.GDIBrushColor.ColorFlags,cfColorAllocated);
end;

{------------------------------------------------------------------------------
  Function: CreateDefaultFont
  Params:  none
  Returns: a Font GDIObject

  Creates an default font, used for initial values
 ------------------------------------------------------------------------------}
function TgtkObject.CreateDefaultFont: PGdiObject;
begin
  Result := NewGDIObject(gdiFont);
  if FDefaultFont = nil then begin
    FDefaultFont:=
      gdk_font_load('-adobe-helvetica-medium-r-normal--*-120-*-*-*-*-iso8859-1');
    if FDefaultFont = nil then begin
      FDefaultFont:= gdk_font_load ('fixed');
      if FDefaultFont = nil then
        raise EOutOfResources.Create('Unable to load default font');
    end;
  end;    
  Result^.GDIFontObject:= FDefaultFont;
  gdk_font_ref(Result^.GDIFontObject);
end;

{------------------------------------------------------------------------------
  Function: CreateDefaultPen
  Params:  none
  Returns: a Pen GDIObject

  Creates an default pen, used for initial values
 ------------------------------------------------------------------------------}
function TgtkObject.CreateDefaultPen: PGdiObject;
begin
//write('  TgtkObject.CreateDefaultPen ->');
  Result := NewGDIObject(gdiPen);
  Result^.GDIPenStyle := PS_SOLID;
  Result^.GDIPenColor.ColorRef := 0;
  Result^.GDIPenColor.Colormap := gdk_colormap_get_system;
  gdk_color_black(Result^.GDIPenColor.Colormap, @Result^.GDIPenColor.Color);
  With Result^.GDIPenColor.Color do
    Result^.GDIPenColor.ColorRef :=
      RGB(GetGValue(Red), GetGValue(Green), GetGValue(Blue));
  Include(Result^.GDIBrushColor.ColorFlags,cfColorAllocated);
end;

{------------------------------------------------------------------------------
  procedure TgtkObject.UpdateDCTextMetric(DC: TDeviceContext);

  Sets the gtk resource file and parses it.
 ------------------------------------------------------------------------------}
procedure TgtkObject.UpdateDCTextMetric(DC: TDeviceContext);
const
  TestString = '{Am|g_}';
var
  XT : TSize;
  dummy: LongInt;
  UseFont : PGDKFont;
  UnRef : Boolean;
  AVGBuffer: array[#32..#126] of char;
  AvgLen: integer;
  c: char;
begin
  with TDeviceContext(DC) do begin
    if dcfTextMetricsValid in DCFlags then begin
      // cache valid
    end else begin
      if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
      then begin
        UseFont := GetDefaultFont;
        UnRef := True;
      end
      else begin
        UseFont := CurrentFont^.GDIFontObject;
        UnRef := False;
      end;
      If UseFont = nil then
        WriteLn('WARNING: [TgtkObject.GetTGextMetrics] Missing font')
      else begin
        FillChar(DCTextMetric, SizeOf(DCTextMetric), 0);
        with DCTextMetric do begin
          IsDoubleByteChar:=FontIsDoubleByteCharsFont(UseFont);
          gdk_text_extents(UseFont, TestString,
            length(TestString), @lbearing, @rBearing, @dummy,
            @TextMetric.tmAscent, @TextMetric.tmDescent);
          for c:=Low(AVGBuffer) to High(AVGBuffer) do
            AVGBuffer[c]:=c;
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
          TextMetric.tmMaxCharWidth :=
            Max(gdk_char_width(UseFont, 'W'),
                gdk_char_width(UseFont, 'M')); // temp hack
          if TextMetric.tmMaxCharWidth<1 then
            TextMetric.tmMaxCharWidth:=1;
          If UnRef then
            GDK_Font_UnRef(UseFont);
        end;
      end;
      Include(DCFlags,dcfTextMetricsValid);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: SetRCFilename
  Params:  const AValue: string
  Returns: none

  Sets the gtk resource file and parses it.
 ------------------------------------------------------------------------------}
procedure TgtkObject.SetRCFilename(const AValue: string);
begin
  if (FRCFilename=AValue) then exit;
  FRCFilename:=AValue;
  FRCFileParsed:=false;
  ParseRCFile;
end;

{------------------------------------------------------------------------------
  procedure TgtkObject.CheckRCFilename;

  Sets the gtk resource file and parses it.
 ------------------------------------------------------------------------------}
procedure TgtkObject.CheckRCFilename;
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
procedure TgtkObject.ParseRCFile;
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
  Function: HashPaintMessage
  Params: a PaintMessage in the Message queue (= PLazQueueItem)
  Returns: a hash index

  Calculates a hash of the handle in the PaintMessage which is used by the
  FPaintMessages (which is a TDynHashArray).
 ------------------------------------------------------------------------------}
function TgtkObject.HashPaintMessage(p: pointer): integer;
var h: integer;
begin
  h:=PMsg(PLazQueueItem(p)^.Data)^.hWnd;
  if h<0 then h:=-h;
  Result:=((h mod 5364329)+(h mod 17)) mod FPaintMessages.Capacity;
end;

{------------------------------------------------------------------------------
  Function: FindPaintMessage
  Params: a window handle
  Returns: nil or a Paint Message to the widget

  Searches in FPaintMessages for a LM_PAINT message with HandleWnd.
 ------------------------------------------------------------------------------}
function TgtkObject.FindPaintMessage(HandleWnd: HWnd): PLazQueueItem;
var h: integer;
  HashItem: PDynHashArrayItem;
begin
  h:=HandleWnd;
  if h<0 then h:=-h;
  h:=((h mod 5364329)+(h mod 17)) mod FPaintMessages.Capacity;
  HashItem:=FPaintMessages.GetHashItem(h);
  if HashItem<>nil then begin
    Result:=PLazQueueItem(HashItem^.Item);
    if PMsg(Result^.Data)^.hWnd=HandleWnd then
      exit;
    HashItem:=HashItem^.Next;
    while (HashItem<>nil) and (HashItem^.IsOverflow) do begin
      Result:=PLazQueueItem(HashItem^.Item);
      if PMsg(Result^.Data)^.hWnd=HandleWnd then
        exit;
      HashItem:=HashItem^.Next;
    end;
  end;
  Result:=nil;
end;

{------------------------------------------------------------------------------
  TgtkObject SetResizeRequest
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
  Therefore LCL size requests for a widget are cached and only the last one is
  sent.
 ------------------------------------------------------------------------------}
procedure TgtkObject.SetResizeRequest(Widget: PGtkWidget);
{$IFDEF VerboseSizeMsg}
var
  LCLControl: TWinControl;
{$ENDIF}
begin
  {$IFDEF VerboseSizeMsg}
  LCLControl:=TWinControl(GetLCLObject(Widget));
  write('PPP TgtkObject.SetResizeRequest Widget=',HexStr(Cardinal(Widget),8));
  if (LCLControl<>nil) then begin
    if LCLControl is TWinControl then
      writeln(' ',LCLControl.Name,':',LCLControl.ClassName)
    else
      writeln(' ERROR: ',LCLControl.ClassName);
  end else begin
    writeln(' ERROR: LCLControl=nil');
  end;
  {$ENDIF}
  if not FWidgetsWithResizeRequest.Contains(Widget) then
    FWidgetsWithResizeRequest.Add(Widget);
end;

{------------------------------------------------------------------------------
  TgtkObject UnsetResizeRequest
  Params: Widget: PGtkWidget

  Unset the mark for the Widget to send a ResizeRequest to the gtk.
  LCL size requests for a widget are cached and only the last one is sent. Some
  widgets like forms send a resize request immediately. To avoid sending resize
  requests multiple times they can unset the mark with this procedure.
 ------------------------------------------------------------------------------}
procedure TgtkObject.UnsetResizeRequest(Widget: PGtkWidget);
begin
  FWidgetsWithResizeRequest.Remove(Widget);
end;

{------------------------------------------------------------------------------
  Function: SetClipboardWidget
  Params: TargetWidget: PGtkWidget - This widget will be connected to all
                  clipboard signals which are all handled by the TGtkObject
                  itself. 
  Returns: none

  All supported targets are added to the new widget. This way, no one,
  especially not the lcl, will notice the change. ;)
 ------------------------------------------------------------------------------}
procedure TgtkObject.SetClipboardWidget(TargetWidget: PGtkWidget);
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
    writeln('  WriteTargetLists WWW START');
    for c:=Low(TClipboardType) to High(TClipboardType) do begin
      TargetList:=gtk_selection_target_list_get(Widget,c);
      writeln('  WriteTargetLists WWW ',ClipboardTypeName[c],' ',TargetList<>nil);
      if TargetList<>nil then begin
        TmpList:=TargetList^.List;
        while TmpList<>nil do begin
          Pair:=PGtkTargetPair(TmpList^.Data);
          writeln('    WriteTargetLists BBB ',Pair^.Target);
          TmpList:=TmpList^.Next;
        end;
      end;
    end;
    writeln('  WriteTargetLists WWW END');
  end;
  {$ENDIF}
  
  {$IFNDEF WIN32}
  procedure ClearTargetLists(Widget: PGtkWidget);
  // MG: Reading in gtk internals is dirty, but there seems to be no other way
  //     to clear the old target lists
  var
    SelectionLists, CurSelList: PGList;
    TargetSelList: PGtkTargetSelectionList;
  begin
    {$IFDEF DEBUG_CLIPBOARD}
    writeln('  ClearTargetLists WWW START');
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
    gtk_object_set_data (PGtkObject(Widget),gtk_selection_handler_key,0);
    {$IFDEF DEBUG_CLIPBOARD}
    writeln('  ClearTargetLists WWW END');
    {$ENDIF}
  end;
  {$ENDIF}
  
var c: TClipboardType;
begin
  if ClipboardWidget=TargetWidget then exit;
  {$IFDEF DEBUG_CLIPBOARD}
  writeln('[TgtkObject.SetClipboardWidget] ',ClipboardWidget<>nil,' -> ',TargetWidget<>nil);
  {$ENDIF}
  if ClipboardWidget<>nil then begin
    {$IFDEF DEBUG_CLIPBOARD}
    WriteTargetLists(ClipboardWidget);
    {$ENDIF}
    {$IFNDEF WIN32}
    ClearTargetLists(ClipboardWidget);
    {$ENDIF}
    {$IFDEF DEBUG_CLIPBOARD}
    WriteTargetLists(ClipboardWidget);
    {$ENDIF}
  end;

  ClipboardWidget:=TargetWidget;
  if ClipboardWidget<>nil then begin
    // connect widget to all clipboard signals
    gtk_signal_connect(PGtkObject(ClipboardWidget),'selection_received',
      TGTKSignalFunc(@ClipboardSelectionReceivedHandler),0);
    gtk_signal_connect(PGtkObject(ClipboardWidget),'selection_get',
      TGTKSignalFunc(@ClipboardSelectionRequestHandler),0);
    gtk_signal_connect(PGtkObject(ClipboardWidget),'selection_clear_event',
      TGTKSignalFunc(@ClipboardSelectionLostOwnershipHandler),0);
    // add all supported targets for all clipboard types
    for c:=Low(TClipboardType) to High(TClipboardType) do begin
      if (ClipboardTargetEntries[c]<>nil) then begin
        {$IFNDEF WIN32}
        gtk_selection_add_targets(ClipboardWidget,ClipboardTypeAtoms[c],
                  ClipboardTargetEntries[c],ClipboardTargetEntryCnt[c]);
        {$ENDIF}
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------
  procedure TGtkObject.WordWrap(AText: PChar; MaxWidthInPixel: integer;
    var Lines: PPChar; var LineCount: integer); virtual;

  Breaks AText into several lines and creates a list of PChar. The last entry
  will be nil.
  Lines break at new line chars and at spaces if a line is longer than
  MaxWidthInPixel or in a word.
  Lines will be one memory block so that you can free the list and all lines
  with FreeMem(Lines).
------------------------------------------------------------------------------}
procedure TGtkObject.WordWrap(DC: HDC; AText: PChar; MaxWidthInPixel: integer;
  var Lines: PPChar; var LineCount: integer);
var
  UseFont : PGDKFont;
  UnRef : Boolean;

  function GetLineWidthInPixel(LineStart, LineLen: integer): integer;
  var
    lbearing, rbearing, width, ascent, descent: LongInt;
  begin
    GetTextExtentIgnoringAmpersands(UseFont, @AText[LineStart], LineLen,
                     @lbearing, @rBearing, @width, @ascent, @descent);
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
        while not (AText[WordEnd] in [' ',#9,#10,#13]) do inc(WordEnd);
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
        UseFont := GetDefaultFont;
        UnRef := True;
      end
      else begin
        UseFont := CurrentFont^.GDIFontObject;
        UnRef := False;
      end;
    end;
  end;

  procedure CleanUpFont;
  begin
    If UnRef then
      GDK_Font_UnRef(UseFont);
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
  CurLineStart:=PChar(Lines)+ArraySize;
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
    RaiseException('TGtkObject.WordWrap Consistency Error:'
      +' Lines+TotalSize<>CurLineStart');
  CurLineEntry[i shr 1]:=nil;

  LinesList.Free;
  CleanUpFont;
end;

function TgtkObject.ForceLineBreaks(DC : hDC; Src: PChar; MaxWidthInPixels : Longint;
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
    Result := StrNew(TmpStr^.str);
end;

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

 { =============================================================================

  $Log$
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
  MG: TNoteBook now starts with no Page and TPage has no auto names

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
  MG: fixed TForm ShowHide repositioning

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
