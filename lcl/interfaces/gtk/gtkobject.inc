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

const
  BOOL_RESULT: array[Boolean] of String = ('False', 'True');
  
{------------------------------------------------------------------------------
  Method: TGtkObject.Create
  Params:  None
  Returns: Nothing

  Contructor for the class.
 ------------------------------------------------------------------------------}
constructor TGtkObject.Create; 
begin
  inherited Create;
  FKeyStateList := TList.Create;
  FDeviceContexts := TDynHashArray.Create(-1);
  FGDIObjects := TDynHashArray.Create(-1);
  FMessageQueue := TLazQueue.Create;
  FPaintMessages := TDynHashArray.Create(-1);
  FPaintMessages.OwnerHashFunction := @HashPaintMessage;
  FWidgetsWithResizeRequest := TDynHashArray.Create(-1);
  FWidgetsResized := TDynHashArray.Create(-1);
  FFixWidgetsResized := TDynHashArray.Create(-1);
  FAccelGroup := gtk_accel_group_new();
  FTimerData  := TList.Create;
  FDefaultFont:= nil;
  FRCFilename := ChangeFileExt(ParamStr(0),'.gtkrc');
  FRCFileParsed := false;
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
    begin
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
        if GDITypeCount[GDIType] > 0 
        then WriteLN(Format('[TgtkObject.Destroy]   %s: %d', [GDITYPENAME[GDIType], GDITypeCount[GDIType]]));
    end;
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
  gtk_accel_group_unref(FAccelGroup);

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
begin
  gtk_window_set_default_size(Window,AWinControl.Width,AWinControl.Height);
  gtk_widget_set_usize(PGtkWidget(Window), -1,-1);
  gtk_widget_set_usize(PGtkWidget(Window),AWinControl.Width,AWinControl.Height);
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
    i: integer;
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
        gtk_widget_set_usize(Widget, LCLControl.Width, LCLControl.Height);
        // move widget on the fixed widget of parent control
        ParentWidget:=pgtkWidget(LCLControl.Parent.Handle);
        ParentFixed := GetFixedWidget(ParentWidget);
        if GtkWidgetIsA(ParentFixed,GTK_FIXED_GET_TYPE)
        or GtkWidgetIsA(ParentFixed,GTK_LAYOUT_GET_TYPE) then begin
          FixedMoveControl(ParentFixed, Widget,
                         LCLControl.Left, LCLControl.Top);
        end else if not (LCLControl.Parent is TNoteBook) then begin
          writeln('WARNING: TgtkObject.SendCachedLCLMessages - no Fixed Widget found');
          writeln('  Control=',LCLControl.Name,':',LCLControl.ClassName);
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
    GtkHeight:=MainWidget^.Allocation.Height;
    
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
    FWidgetsResized.AssignTo(List);
    for i:=0 to List.Count-1 do begin
      MainWidget:=List[i];
      if not FWidgetsWithResizeRequest.Contains(MainWidget) then begin
        SendSizeNotificationToLCL(MainWidget);
        FixWidget:=GetFixedWidget(MainWidget);
      end;
    end;
    
    { if any client area was resized, which MainWidget Size was already in sync
      with the LCL, no message was send. So, tell each changed client area to
      check its size.
    }
    {$IFDEF VerboseSizeMsg}
    writeln('HHH3 SendCachedGtkClientResizeNotifications Updating ClientRects ...');
    {$ENDIF}
    FFixWidgetsResized.AssignTo(List);
    for i:=0 to List.Count-1 do begin
      FixWidget:=List[i];
      MainWidget:=GetMainWidget(FixWidget);
      LCLControl:=TWinControl(GetLCLObject(MainWidget));
      LCLControl.DoAdjustClientRectChange;
    end;
    
    List.Free;
    FWidgetsResized.Clear;
    FFixWidgetsResized.Clear;
    {$IFDEF VerboseSizeMsg}
    writeln('HHH4 SendCachedGtkClientResizeNotifications  completed.');
    {$ENDIF}
  end;

begin
  SendCachedGtkResizeNotifications;
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
  SendCachedLCLMessages;
  gtk_main_iteration_do(True);
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.AppTerminate
  Params:  None
  Returns: Nothing

  *Note: Tells GTK Engine to halt and destroy
 ------------------------------------------------------------------------------}
procedure TGtkObject.AppTerminate;

  procedure DestroyGdkCursor(var Cursor: pGDKCursor);
  begin
    gdk_Cursor_Destroy(Cursor);
    Cursor:=nil;
  end;
  
  procedure DeleteAndNilObject(var h: HGDIOBJ);
  begin
    DeleteObject(h);
    h:=0;
  end;

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

  DestroyGdkCursor(Cursor_Watch);
  DestroyGdkCursor(Cursor_Arrow);
  DestroyGdkCursor(Cursor_Cross);
  DestroyGdkCursor(Cursor_Hand1);
  DestroyGdkCursor(Cursor_XTerm);
  DestroyGdkCursor(Cursor_StdArrow);
  DestroyGdkCursor(Cursor_HSplit);
  DestroyGdkCursor(Cursor_VSplit);
  DestroyGdkCursor(Cursor_SizeNWSE);
  DestroyGdkCursor(Cursor_SizeNS);
  DestroyGdkCursor(Cursor_SizeNESW);
  DestroyGdkCursor(Cursor_SizeWE);
  DestroyGdkCursor(Cursor_Help);
  DestroyGdkCursor(Cursor_SizeAll);

  gtk_object_unref(PGTKObject(FGTKToolTips));
  FGTKToolTips := nil;
   
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

  // MG: using gtk_main_quit is not a clean way to close
  //gtk_main_quit;
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.Init
  Params:  None
  Returns: Nothing

  *Note: Initialite GTK engine
 ------------------------------------------------------------------------------}
procedure TGtkObject.Init;
var
  LogBrush: TLogBrush;
  logPen : TLogPen;
  //Attributes: TGdkWindowAttr;
  //AttributesMask: gint;
  FG,BG : TgdkColor;  //foreground and background
begin
  // initialize app level gtk engine 
  gtk_set_locale ();

  // call init and pass cmd line args
  gtk_init (@argc, @argv);
  
  // read gtk rc file
  FRCFileParsed:=true;
  ParseRCFile;

  // Initialize Stringlist for holding styles
  Styles := TStringlist.Create;
  
  // Create default cursor types
  Cursor_Watch := gdk_cursor_new(gdk_Watch);
  Cursor_Arrow := gdk_cursor_new(gdk_Arrow);
  Cursor_Cross := gdk_cursor_new(gdk_Cross);
  Cursor_Hand1 := gdk_cursor_new(gdk_Hand1);
  Cursor_XTerm := gdk_cursor_new(gdk_XTerm);
  Cursor_StdArrow := gdk_cursor_new(GDK_LEFT_PTR);
  Cursor_HSplit := gdk_cursor_new(GDK_SB_H_DOUBLE_ARROW);
  Cursor_VSplit := gdk_cursor_new(GDK_SB_V_DOUBLE_ARROW);
  Cursor_SizeNWSE := gdk_cursor_new(GDK_TOP_LEFT_CORNER);
  Cursor_SizeNS := gdk_cursor_new(GDK_SB_V_DOUBLE_ARROW);
  Cursor_SizeNESW := gdk_cursor_new(GDK_BOTTOM_LEFT_CORNER);
  Cursor_SizeWE := gdk_cursor_new(GDK_SB_H_DOUBLE_ARROW);
  Cursor_SizeAll:= gdk_cursor_new(GDK_FLEUR);
  Cursor_Help := gdk_cursor_new(GDK_QUESTION_ARROW);

  gtk_key_snooper_install(@GTKKeySnooper, @FKeyStateList);

  // Init tooltips
  FGTKToolTips := gtk_tooltips_new;   
  gtk_object_ref(PGTKObject(FGTKToolTips));
  gtk_toolTips_Enable(FGTKToolTips);
  FG.red := $FFFF;
  FG.Green := $FFFF;
  fg.blue := $FFFF;
  BG.REd := $4444;
  bg.Green := $ffff;
  bg.Blue := $1111;
  
  gtk_toolTips_set_colors(FGTKToolTips,@FG,@BG);

  // Init stock objects;
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

  // clipboard
  ClipboardTypeAtoms[ctPrimarySelection]:=GDK_SELECTION_PRIMARY;
  ClipboardTypeAtoms[ctSecondarySelection]:=GDK_SELECTION_SECONDARY;
  ClipboardTypeAtoms[ctClipboard]:=gdk_atom_intern('CLIPBOARD',0);
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
    // this will destroy all child handles
    aWinControl.Parent := nil;
    aWinControl.Parent := aParent;
  end;
  ResizeChild(Sender,aWinControl.Left,aWinControl.Top,
                     aWinControl.Width,aWinControl.Height);
  ShowHide(Sender);
  Result:=0;
End;

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

    TMP := CreateCompatibleBitmap(-1, Width, Height);
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

{------------------------------------------------------------------------------
  function GetComboBoxItemIndex(ComboBox: TComboBox): integer;

  Returns the current ItemIndex of a TComboBox
 ------------------------------------------------------------------------------}
function GetComboBoxItemIndex(ComboBox: TComboBox): integer;
begin
  Result:=ComboBox.Items.IndexOf(ComboBox.Text);
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
  AddItemListItem : TListItem;
  Rect      : TRect;
  FormIconGdiObject: PGdiObject;     // currently only used by LM_SETFORMICON
  Count     : Integer;               //Used in TListView LM_LV_CHANGEITEM
  Titles    : Array [0..255] of PChar;
  BitImage  : TBitmap;
  Geometry  : TGdkGeometry;
  Accel     : integer;
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

    LM_ShowHide : ShowHide(Sender);

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
            if Not Assigned(AParent) then
               Begin
                 Assert(true, Format('Trace: [TgtkObject.IntSendMessage3] %s --> Parent is not assigned', [Sender.ClassName]));

               end
               else
               Begin
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
      begin
        if (Sender is TListView) then
        begin
          Widget:= GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget;

          Num := Integer(data^);
          AddItemListItem := TListview(sender).Items[Num];
          pStr := StrAlloc(length(AddItemListItem.Caption) + 1);
          StrPCopy(pStr, AddItemListItem.Caption);
          gtk_clist_set_text(PgtkCList(Widget),num,0,pStr);
          if (TListview(sender).SmallImages <> nil)
          and (TListItem(TListview(sender).Items[Num]).ImageIndex > -1) 
          then begin
            if (TListItem(TListview(sender).Items[Num]).ImageIndex
              < TListview(sender).SmallImages.Count)
            then begin
              //draw image
              BitImage := TBitmap.Create;
              TListview(sender).SmallImages.GetBitmap(
                TListItem(TListview(sender).Items[Num]).ImageIndex,BitImage);
              {$IfNDef Win32}
              gtk_clist_set_pixtext(Pgtkclist(Widget),Num,0,pStr,3,
                pgdkPixmap(PgdiObject(BitImage.handle)^.GDIBitmapObject),nil);
              {$EndIF}
            end;
          end;
          StrDispose(pStr);

          for count := 0 to AddItemListItem.SubItems.Count-1 do
          begin
            pStr := StrAlloc(length(AddItemListItem.SubItems.Strings[Count]) + 1);
            StrPCopy(pStr, AddItemListItem.SubItems.Strings[Count]);
            gtk_clist_set_text(PgtkCList(Widget),num,count+1,pStr);
            StrDispose(pStr);
          end;
        end;
      end;
      
      LM_LV_ADDITEM :
      if (Sender is TListView) then
      begin
        Widget:= GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget;

        //get last item and add it..
        pStr := StrAlloc(length('Test') + 1);
        StrPCopy(pStr, 'Test');
        
        // ToDo: make this dynamic
        
        Titles[0] := pStr;
        for Count := 1 to 255 do
          Titles[Count] := nil;
        Num := gtk_clist_append(PgtkCList(Widget),@Titles);
        StrDispose(pStr);

        AddItemListItem := TListView(sender).Items[TListView(sender).Items.Count-1];
        if AddItemListItem <> nil then
          Begin
           gtk_clist_set_text(PgtkCList(Widget),num,0,@AddItemListItem.Caption);
          end;
      end;

      LM_LV_SELECTITEM:
      if (Sender is TListView) then
      begin
        Widget:= GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget;
        gtk_clist_unselect_all(PGtkCList(Widget));
        
        writeln('LM_LV_SELECTITEM ',TListItem(Data).Index);
        
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
        Begin
          Widget := PgtkWidget(TCustomForm(Sender).Handle);
          gdk_window_raise(GetControlWindow(Widget));
        end;
      end;

      LM_BTNDEFAULT_CHANGED :
      Begin
        if (TButton(Sender).Default) and (GTK_WIDGET_CAN_DEFAULT(pgtkwidget(handle))) 
           then gtk_widget_grab_default(pgtkwidget(handle))
           else gtk_widget_Draw_Default(pgtkwidget(Handle));  //this isn't right but I'm not sure what to call
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
           if Accel <> GDK_VOIDSYMBOL then begin
             gtk_accel_group_add(gtk_accel_group_get_default, Accel, 0,
               GTK_ACCEL_VISIBLE, PGtkObject(TBitBtn(Sender).Handle), 'clicked');
           end;
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
        if GTK_WIDGET_CAN_FOCUS(PgtkWidget(Handle)) then
          gtk_widget_grab_focus(PgtkWidget(handle))
        else
          Writeln('The control '+TControl(Sender).name+' can not get focus');
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

          If (Sender <> Application.MainForm)
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
            if (FormIconGdiObject<>nil)
            and (GetControlWindow(PGtkWidget(Handle))<>nil)
            then begin
              gdk_window_set_icon(GetControlWindow(PgtkWidget(Handle)), nil,
                FormIconGdiObject^.GDIBitmapObject,
                FormIconGdiObject^.GDIBitmapMaskObject);
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
              Data:= TGtkListStringList.Create(PGtkList(Widget));
              Result:= Integer(Data);
            end;
        else 
          raise Exception.Create('Message LM_GETITEMS - Not implemented');
        end;
      end;

      LM_GETTEXT :
      begin
        Assert (true, 'WARNING:[TgtkObject.IntSendMessage3] usage of LM_GETTEXT superfluous, use interface-function GetText instead'); 
        Result := integer (nil);
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
            gtk_list_select_item(PGTKLIST(PGTKCOMBO(Handle)^.list), Integer(Data));
                                 
          csListBox :
            begin
              gtk_list_select_item(PGtkList(GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget), Integer(Data));
            end;
            
          csCListBox:
            gtk_clist_select_row(PGtkCList(GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget),
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
            Result:= gtk_editable_get_position(PGtkEditable(
                                               PGtkCombo(Handle)^.entry)) + 1;
          csEdit:
            Result:= gtk_editable_get_position(PGtkEditable(Handle)) + 1;
          end;
        end;
      end;
          
      LM_GETSELLEN :
      begin
        if (Sender is TControl) then begin
          case TControl(Sender).fCompStyle of
          csComboBox:
            Result:= PGtkEditable(PGtkCombo(Handle)^.entry)^.selection_end_pos
                     - PGtkEditable(PGtkCombo(Handle)^.entry)^.selection_start_pos;
          csEdit:
            Result:= PGtkEditable(Handle)^.selection_end_pos
                     - PGtkEditable(Handle)^.selection_start_pos;
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
	        csEdit:
            gtk_editable_set_position(PGtkEditable(Handle), Integer(Data));
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
          csListBox : Result:= g_list_length(PGtkList(GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget)^.selection);
          csCListBox: Result:= g_list_length(PGtkCList(GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget)^.selection);
        end;
      end;
  
      LM_GETSEL :
      begin
         if (Sender as TWinControl).fCompStyle = csListBox then
         begin
           { Get the child in question of that index }
           ListItem:= g_list_nth_data(PGtkList(GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget)^.children, Integer(Data^));
           Result:= g_list_index(PGtkList(GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget)^.selection, ListItem);
         end
         else if (Sender as TControl).fCompStyle = csCListBox then
         begin
           { Get the selections }
           GList:= PGtkCList(GetWidgetInfo(Pointer(Handle), True)^.ImplementationWidget)^.selection;
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
        case TControl(Sender).fCompStyle of
          csBitBtn,
          csButton,
          csToolButton:
            Accelerate(Widget, TLMShortcut(data^), 'clicked');
          else
            Accelerate(Widget, TLMShortcut(data^), 'activate_item');
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
            if Sender is TCustomForm then
              writeln('LM_SETGEOMETRY ',Sender.ClassName);
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
function TGtkObject.GetText(Sender: TControl; var Text: String): Boolean;
var
  CS: PChar;
begin
  Result := True;
  case Sender.fCompStyle of
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
Assert(False, 'Trace:ADDCHILD');
   pFixed := GetFixedWidget(PGtkWidget(Parent));
   if pFixed <> Parent then
     FixedPutControl(pFixed, Child, Left, Top);
//   gtk_object_set_data(PgtkObject(Child),'Owner',Parent);
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
      csStatusBar : begin
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
        gtk_label_parse_uline(PGtkLabel(Child), aLabel);
      Finally
        StrDispose(aLabel);
      end;
    end;

  csForm, 
  csFileDialog, 
  csColorDialog,
  csFontDialog  : gtk_window_set_title(pGtkWindow(p),PLabel);

  csLabel       :
    begin
      AccelKey:= GetAccelKey(p);
      if AccelKey <> 0 then
        gtk_widget_remove_accelerators(p, 'grab_focus', false);
		    
      if TLabel(Sender).ShowAccelChar then begin
	aLabel:= Ampersands2Underscore(pLabel);
	try
          AccelKey:= gtk_label_parse_uline(pGtkLabel(p), aLabel);
	  SetAccelKey(p, AccelKey);
	  if AccelKey <> 0 then
	    gtk_widget_add_accelerator(p, 'grab_focus', gtk_accel_group_get_default(),
	      AccelKey, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
        finally
	  StrDispose(aLabel);
	end;
      end else begin
        gtk_label_set_text(PGtkLabel(p), pLabel);
        gtk_label_set_pattern(PGtkLabel(p), nil);
      end;
    end;

  csCheckBox    : gtk_label_set_text(
                    pGtkLabel(pgtkCheckButton(p)^.Toggle_Button.Button.Child),
                    pLabel);

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
      //writeln('SetLabel: ',PLabel);
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
  {$IfNdef Win32}
  if Sender is TWinControl
  then with TWinControl(Sender) do
  begin
    // Temphack to set backcolor, till better solution
    if HandleAllocated and ((Color and SYS_COLOR_BASE)=0) then begin
      Widget:=PGtkWidget(Handle);
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


  // OBSOLETE
  
   //NOT USED RIGHT NOW..........CAUSES ALL FORMS TO USE THESE COLORS!!!!!!
   {
   widget := TCustomForm(Sender).handle;
   TheStyle := pgtkWidget(widget)^.thestyle;
   NewColor := ConvertTogdk(TCustomForm(Sender).Color);
   gdk_color_alloc (gdk_colormap_get_system (), @NewColor);
   gdk_gc_set_foreground (TheStyle^.fg_gc[GTK_STATE_NORMAL], @NewColor);
   gdk_gc_set_background (TheStyle^.fg_gc[GTK_STATE_NORMAL], @NewColor);
   }
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
      end else
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
        ConnectSenderSignal(PgtkObject(PgtkCombo(gObject)^.entry), 'focus-in-event', @gtkFocusCB);
        ConnectSenderSignal(PgtkObject(PgtkCombo(gObject)^.entry), 'focus-out-event', @gtkKillFocusCB);
        ConnectSenderSignal(PgtkObject(PgtkCombo(gObject)^.list), 'focus-in-event', @gtkFocusCB);
        ConnectSenderSignal(PgtkObject(PgtkCombo(gObject)^.list), 'focus-out-event', @gtkKillFocusCB);
      end else begin
        ConnectSenderSignal(gObject, 'focus-in-event', @gtkFocusCB);
        ConnectSenderSignal(gObject, 'focus-out-event', @gtkKillFocusCB);
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
        ConnectSenderSignal(gObject, 'key-press-event', @GTKKeyUpDown, GDK_KEY_PRESS_MASK);
        ConnectSenderSignal(gObject, 'key-release-event', @GTKKeyUpDown, GDK_KEY_RELEASE_MASK);
      end
      else
      if (Sender is TCustomListBox) then Begin
      
        //TODO:listbox is STILL not sendig keypress events even with these lines.

        ConnectSenderSignal(gCore, 'key-press-event', @GTKKeyUpDown, GDK_KEY_PRESS_MASK);
        ConnectSenderSignal(gCore, 'key-release-event', @GTKKeyUpDown, GDK_KEY_RELEASE_MASK);
      end;
      ConnectSenderSignal(gFixed, 'key-press-event', @GTKKeyUpDown, GDK_KEY_PRESS_MASK);
      ConnectSenderSignal(gFixed, 'key-release-event', @GTKKeyUpDown, GDK_KEY_RELEASE_MASK);
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
        ConnectSenderSignal(PgtkObject(PgtkCombo(gObject)^.entry), 'motion-notify-event',
          @GTKMotionNotify, GDK_POINTER_MOTION_MASK);
        ConnectSenderSignalAfter(PgtkObject(PgtkCombo(gObject)^.entry), 'motion-notify-event',
          @GTKMotionNotifyAfter, GDK_POINTER_MOTION_MASK);

        ConnectSenderSignal(PgtkObject(PgtkCombo(gObject)^.button), 'motion-notify-event',
          @GTKMotionNotify, GDK_POINTER_MOTION_MASK);
        ConnectSenderSignalAfter(PgtkObject(PgtkCombo(gObject)^.button), 'motion-notify-event',
          @GTKMotionNotifyAfter, GDK_POINTER_MOTION_MASK);
      end
      else begin
        ConnectSenderSignal(gFixed, 'motion-notify-event', @GTKMotionNotify,
                      GDK_POINTER_MOTION_MASK);
        ConnectSenderSignalAfter(gFixed, 'motion-notify-event', @GTKMotionNotifyAfter,
                      GDK_POINTER_MOTION_MASK);
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
        ConnectSenderSignalAfter(gFixed, 'button-press-event', @gtkMouseBtnPressAfter,
                      GDK_BUTTON_PRESS_MASK);
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
      if sender is TButton then
        ConnectSenderSignal(gObject, 'enter', @gtkenterCB)
      else
        ConnectSenderSignal(gObject, 'focus-in-event', @gtkFocusInNotifyCB); //TODO: check this focus in is mapped to focus
    end;

    LM_EXIT :
    begin
      if sender is TButton then
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
      ConnectSenderSignal(gObject, 'insert-text', @gtkinserttext);
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
      ConnectSenderSignal(gObject, 'abort-column-resize', @gtkLVAbortColumnResize);
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
  gObject : gtk_Object;
begin
  gObject := ObjectToGTKObject(Sender);
  if gObject = nil then Exit;

  if not (Sender is TMenuItem) then
    gtk_signal_handlers_destroy(gObject);
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
  Accelerators: PGSlist;
  AccelEntry : PGtkAccelEntry;
begin
  Handle := hwnd(ObjectToGtkObject(Sender));
  if Handle=0 then exit;
  Widget:=PGtkWidget(Handle);

  // Remove control accelerators - has to be done due to GTK+ bug?
  if Sender is TWinControl then begin
    Accelerators:= gtk_accel_group_entries_from_object(PGtkObject(Handle));
    while Accelerators <> nil do begin
      AccelEntry:= Accelerators^.data;
      Accelerators:= Accelerators^.next;
      with AccelEntry^ do 
        gtk_accel_group_remove(accel_group, accelerator_key, accelerator_mods, PGtkObject(Handle));
    end;
  end;  

  RemoveCallbacks(Sender);

  FixWidget:=GetFixedWidget(Widget);
  SetWidgetIsDestroyingHandle(Widget);

  // remove pending size messages
  FWidgetsWithResizeRequest.Remove(Widget);
  FWidgetsResized.Remove(Widget);
  FFixWidgetsResized.Remove(FixWidget);

  //writeln('>>> LM_DESTROY ',Sender.Classname,' Sender=',HexStr(Cardinal(Sender),8),' Handle=',HexStr(Cardinal(Handle),8));
  // update mouse caturing
  if MCaptureHandle=Handle then Uncapturehandle;
  
  // update clipboard widget
  if ClipboardWidget=Widget then
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

  // destroy the widget
  DestroyWidget(Widget);

  //writeln('>>> LM_DESTROY END ',Sender.Classname,' Sender=',HexStr(Cardinal(Sender),8),' Handle=',HexStr(Cardinal(Handle),8));
  // remove all remaining messages to this component
  QueueItem:=FMessageQueue.First;
  while (QueueItem<>nil) do begin
    MsgPtr := PMsg(QueueItem^.Data);
    if (MsgPtr^.HWnd=Handle) then begin
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

        csComboBox,csNotebook,csTrackBar :
        Begin
          SetCallback(LM_CHANGED,Sender);
        End;

        csEdit,csMemo:
        begin
          SetCallback(LM_CHANGED,Sender);
          SetCallback(LM_ACTIVATE, Sender);
          SetCallback(LM_CUTTOCLIP,Sender);
          SetCallback(LM_COPYTOCLIP,Sender);
          SetCallback(LM_PASTEFROMCLIP,Sender);
        End;
        
        csFixed :
        Begin
          SetCallback(LM_HSCROLL,Sender);
          SetCallback(LM_VSCROLL,Sender);
        end;

        csForm:
        Begin
          SetCallback(LM_CONFIGUREEVENT,Sender);
          SetCallback(LM_CLOSEQUERY,Sender);
          SetCallBack(LM_Activate,Sender);
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
  ItemList:= TGtkListStringList.Create(PGtkList(Widget^.List));
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

{------------------------------------------------------------------------------
  Function: TGTKObject.CreateComponent
  Params:   sender - object for which to create visual representation
  Returns:  nothing

  Tells GTK Engine to create a widget
 ------------------------------------------------------------------------------}
procedure TgtkObject.CreateComponent(Sender : TObject);
const 
  FormStyleMap : array[TFormBorderStyle] of integer = (
    GTK_WINDOW_DIALOG,  // bsNone
    GTK_WINDOW_TOPLEVEL,// bsSingle
    GTK_WINDOW_TOPLEVEL,// bsSizeable
    GTK_WINDOW_DIALOG,  // bsDialog
    GTK_WINDOW_DIALOG,  // bsToolWindow
    GTK_WINDOW_DIALOG   // bsSizeToolWin
  );
  FormResizableMap : array[TFormBorderStyle] of gint = (
    0, // bsNone
    1, // bsSingle
    1, // bsSizeable
    0, // bsDialog
    0, // bsToolWindow
    1  // bsSizeToolWin
  );
//unused:  FormBorderWidth : array[TFormBorderStyle] of gint = (0, 1, 2, 1, 1, 2);  

//unused:type
//unused:   Tpixdata = Array[1..20] of String;

var
   Caption : ansistring;          // the caption of "Sender"
   StrTemp : PChar;               // same as "caption" but as PChar
   TempWidget,
   TempWidget2 : PGTKWidget;       // pointer to gtk-widget (local use when neccessary)
   p          : pointer;          // ptr to the newly created GtkWidget
   CompStyle,                     // componentstyle (type) of GtkWidget which will be created
   TempInt   : Integer;           // local use when neccessary
   Adjustment: PGTKAdjustment;    // currently only used for csFixed
   // - for csBitBtn
   Box       : Pointer;           // currently only used for TBitBtn and TForm and TListView
   //pixmap    : pGdkPixMap;        // TBitBtn - the default pixmap
   pixmapwid : pGtkWidget;        // currently only used for TBitBtn
   //mask      : pGDKBitmap;        // currently only used for TBitBtn
   //style     : pgtkStyle;         // currently only used for TBitBtn
   label1    : pgtkwidget;        // currently only used for TBitBtn
   //TempStr   : String;            // currently only used for TBitBtn to load default pixmap
   //pStr      : PChar;             // currently only used for TBitBtn to load default pixmap
   ParentForm: TCustomForm;
   Accel : PChar;
   SetupProps : boolean;
   Titles: PPGChar;

  procedure Set_RC_Name(AWidget: PGtkWidget);
  var RCName: string;
    AComponent: TComponent;
  begin
    if (AWidget=nil) or (not (Sender is TComponent)) then exit;

    // check if a unique name can be created
    AComponent:=TComponent(Sender);
    while (AComponent<>nil) and (AComponent.Name<>'') do begin
      AComponent:=AComponent.Owner;
    end;
    if (AComponent=nil) or (AComponent=TComponent(Application)) then begin
      // create unique name
      AComponent:=TComponent(Sender);
      RCName:=AComponent.Name;
      while (AComponent<>nil) do begin
        AComponent:=TComponent(AComponent.Owner);
        if (AComponent<>nil) and (AComponent.Name<>'') then
          RCName:=AComponent.Name+'_'+RCName;
      end;
      gtk_widget_set_name(AWidget,PChar(RCName));
      gtk_widget_set_rc_style(AWidget);
    end;
    if (Sender is TCustomForm)
    and ((Application.MainForm=TCustomForm(Sender))
      or (Application.MainForm=nil))
    then
      UpdateSysColorMap(AWidget);
  end;

begin
  p := nil;
  SetupProps:= false;

  CompStyle := csNone;
  Caption   := Sender.ClassName;
  if (Sender is TControl) then
  begin
    Caption := TControl(Sender).Caption;
    CompStyle := TControl(Sender).FCompStyle
  end
  else
  if (Sender is TMenuItem) then
  begin
    Caption := TMenuItem(Sender).Caption;
    CompStyle := TMenuItem(Sender).FCompStyle;
  end
  else
  if (Sender is TMenu) or (Sender is TPopupMenu)
  then
    CompStyle := TMenu(Sender).FCompStyle
  else
  if (Sender is TCommonDialog)
  then
    CompStyle := TCommonDialog(Sender).FCompStyle;

  if Caption = '' then Caption := 'Blank';

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
      Accel := Ampersands2Underscore(StrTemp);
      p := gtk_button_new_with_label(Accel);
      //If Accel <> -1 then
        With PGTKButton(P)^ do
          gtk_label_parse_uline(PGtkLabel(Child), Accel);
      StrDispose(Accel);
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
                                                    (Width-10) div ListColumns);
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

  csFixed: //used for TWinControl, maybe change this to csWinControl
    begin
      p := GTKAPIWidget_New;
      gtk_scrolled_window_set_policy(PGTKScrolledWindow(p),
        GTK_POLICY_NEVER, GTK_POLICY_NEVER);

      Adjustment := gtk_scrolled_window_get_vadjustment(PGTKScrolledWindow(p));
      if Adjustment <> nil
      then with Adjustment^ do
      begin
        gtk_object_set_data(PGTKObject(Adjustment), 'ScrollBar',
                            PGTKScrolledWindow(p)^.VScrollBar);
        Step_Increment := 1;
      end;
      Adjustment := gtk_scrolled_window_get_hadjustment(PGTKScrolledWindow(p));
      if Adjustment <> nil
      then with Adjustment^ do
      begin
        gtk_object_set_data(PGTKObject(Adjustment), 'ScrollBar',
                            PGTKScrolledWindow(p)^.HScrollBar);
        Step_Increment := 1;
      end;
    end;

  csForm :
    begin
      Assert(Sender is TForm);
      p := gtk_window_new(FormStyleMap[TForm(Sender).BorderStyle]);

      gtk_window_set_policy (GTK_WINDOW (p), FormResizableMap[TForm(Sender).BorderStyle],
        FormResizableMap[TForm(Sender).BorderStyle], 0);
      gtk_window_set_title(pGtkWindow(p), strTemp);

      // the clipboard needs a widget
      if ClipboardWidget=nil then
        SetClipboardWidget(p);

      // Create the VBox, we need that to place controls outside
      // the client area (like menu and the statusbar)
      Box := gtk_vbox_new(False, 0);
      gtk_container_add(p, Box);
      gtk_widget_show(Box);

      // Create the form client area
      TempWidget := gtk_fixed_new();
      gtk_box_pack_end(Box, TempWidget, True, True, 0);
      gtk_widget_show(TempWidget);
      SetFixedWidget(p, TempWidget);
      SetMainWidget(p, TempWidget);

      //drag icons
      if Drag_Icon = nil then
        Drag_Icon := gdk_pixmap_colormap_create_from_xpm_d (nil,
                             gtk_widget_get_colormap (p), @Drag_Mask,
                             nil, @IMGDrag_Icon);
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
       p := gtk_window_new(gtk_window_Popup);
       gtk_window_set_policy (GTK_WINDOW (p), 0, 0, 0);

       // Create the form client area
       TempWidget := gtk_fixed_new();
       gtk_container_add(p, TempWidget);
       gtk_widget_show(TempWidget);
       SetFixedWidget(p, TempWidget);
       SetMainWidget(p, TempWidget);
      //Try to make all hint forms transient to the main form
       If Application.MainForm <> nil then
         gtk_window_set_transient_for(PGTKWindow(P), PGTKWindow(Application.MainForm.Handle));
       gtk_widget_show(p);
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
        GetMem(Titles,SizeOf(PChar)*2);
        for TempInt:=0 to Items.Count-1 do begin
          if Items[TempInt].Caption<>'' then
            Titles[0] := PChar(Items[TempInt].Caption)
          else
            Titles[0] := #0;
          Titles[1] := nil;
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
      Box := PGTKBin(ParentForm.Handle)^.Child;
      gtk_box_pack_start(Box, p, False, False, 0);
      SetAccelGroup(p, gtk_accel_group_get_default);
      gtk_widget_show(p);
    end;

  csMemo :
    begin
      P := gtk_scrolled_window_new(nil, nil);
      TempWidget := gtk_text_new(nil, nil);
      gtk_container_add(p, TempWidget);
      gtk_text_set_adjustments(PGtkText(TempWidget), PGtkAdjustment(PGtkScrolledWindow(P)^.hscrollbar),
        PGtkAdjustment(PGtkScrolledWindow(P)^.vscrollbar));

      SetMainWidget(p, TempWidget);
      GetWidgetInfo(p, True)^.ImplementationWidget := TempWidget;

      gtk_widget_show(P);
      gtk_widget_show(TempWidget);
      
      SetupProps:= true;
    end;

  csMenuBar :
    begin
       P := gtk_menu_bar_new();
       gtk_container_add(
         GTK_Container(
           GetFixedWidget(Pointer(TWinControl(TMenu(Sender).Owner).Handle))), P);
       SetAccelGroup(p, gtk_accel_group_get_default);
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
      if TempWidget <> nil then
        P:= gtk_radio_button_new_with_label(PGtkRadioButton(TempWidget)^.group, StrTemp)
      else  
        P:= gtk_radio_button_new_with_label(nil, StrTemp);
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
      Accel := Ampersands2Underscore(StrTemp);
      //p := gtk_button_new_with_label(StrTemp);
      p := gtk_button_new_with_label(Accel);
      if TToolButton(Sender).Style = tbsButton then
      Begin
        //If Accel <> -1 then
          With PGTKButton(P)^ do
            gtk_label_parse_uline(PGtkLabel(Child), Accel);
      end;
      StrDispose(Accel);
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

  // MWE: next will be obsoleted by WinWidgetInfo
  //Set these for functions like GetWindowLong  Added 01/07/2000
  if p <> nil then
  Begin
    SetLCLObject(p, Sender);
    gtk_object_set_data(pgtkObject(p),'Style',0);
    gtk_object_set_data(pgtkObject(p),'ExStyle',0);
  end;
  //--------------------------

  if (Sender is TWinControl) then
  begin
    TWinControl(Sender).Handle := THandle(p);
    if p <> nil then begin
      gtk_object_set_data(pgtkobject(p),'Sender',Sender);
      SetResizeRequest(p);
    end;
  end
  else
  if (Sender is TMenuItem) then
    TMenuItem(Sender).Handle := HMenu(p)
  else
  if (Sender is TMenu) then
    TMenu(Sender).Items.Handle := HMenu(p)
  else
  if (Sender is TCommonDialog) then
    TCommonDialog(Sender).Handle:= THandle(p);


  Set_RC_Name(p);

  if SetupProps then SetProperties(Sender);

  StrDispose(StrTemp);
  if P <> nil then begin
    {$IfNDef win32}
    if Sender is TCustomForm then
      gtk_widget_set_app_paintable(p,true);
    {$EndIf}
    HookSignals(Sender);
  end;
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
var FormIconGdiObject: PGDIObject;
  SenderWidget, ParentFixed, ParentWidget: PGTKWidget;
  LCLControl: TWinControl;
  Decor, Func : Longint;
begin
  LCLControl:=TWinControl(Sender);
  SenderWidget:=PgtkWidget(LCLControl.Handle);
  //if Sender is TForm then
  //  writeln('[TgtkObject.ShowHide] START ',Sender.ClassName,' Visible=',TControl(Sender).Visible);
  if LCLControl.Visible
  or ((csDesigning in LCLControl.ComponentState)
    and not (csNoDesignVisible in LCLControl.ControlStyle)) then
  begin
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

    if (Sender is TCustomForm) then
      If (TCustomForm(Sender).BorderStyle <> bsSizeable) or
        (TCustomForm(Sender).FormStyle = fsStayOnTop)
      then begin
        Decor := GetWindowDecorations(TCustomForm(Sender));
        Func := GetWindowFunction(TCustomForm(Sender));
        gtk_widget_realize(SenderWidget);
        gdk_window_set_decorations(GetControlWindow(SenderWidget), decor);
        gdk_window_set_functions(GetControlWindow(SenderWidget), func);
      end;
    gtk_widget_show(SenderWidget);
    SetColor(Sender);
    if (Sender is TCustomForm) and (GetControlWindow(SenderWidget)<>nil) then begin
      FormIconGdiObject:=PGDIObject(TCustomForm(Sender).GetIconHandle);
      if (FormIconGdiObject<>nil) then begin
        gdk_window_set_icon(GetControlWindow(SenderWidget), nil,
                FormIconGdiObject^.GDIBitmapObject,
                FormIconGdiObject^.GDIBitmapMaskObject);
      end;
    end;
  end
  else Begin
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
    TheCaption := StrAlloc(Length(ThePage.Caption) + 1);
    try
      StrPCopy(TheCaption, ThePage.Caption);
      
      gtk_label_set_text(PGtkLabel(TabLabelWidget),TheCaption);
      if MenuLabelWidget<>nil then
        gtk_label_set_text(PGtkLabel(MenuLabelWidget),TheCaption);
    finally
      StrDispose(TheCaption);
    end;
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

  gdk_draw_pixmap(fwindow,
    PGtkStyle(widget^.TheStyle)^.fg_gc[GTK_WIDGET_STATE (widget)],
    pixmap,
    0,0,
    0,0,
    pgtkwidget(widget)^.allocation.width, pgtkwidget(widget)^.allocation.height);
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
  Widget := PgtkWidget(aDC.HWnd);

  Image := gtk_Object_get_data(pgtkobject(widget),'Image');
  if Image = nil then begin
    Image := gdk_image_get(GetControlWindow(widget),0,0,
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
begin
  aDC := TDeviceContext(TCanvas(Sender).Handle);
  if aDC = nil then exit;
  Widget := PgtkWidget(aDC.HWnd);

  Image := gtk_Object_get_data(pgtkobject(Widget),'Image');
  if Image = nil then begin
    Image := gdk_image_get(GetControlWindow(widget),0,0,
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
    SelectionColor[0]:=(Color and $ff)/255;
    SelectionColor[1]:=((Color shr 8) and $ff)/255;
    SelectionColor[2]:=((Color shr 16) and $ff)/255;
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
    then TCheckBoxState (data^) := cbChecked
    else TCheckBoxState (data^) := cbUnChecked;
                    
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
          gtk_entry_set_visibility(PGtkEntry(Handle), TCustomEdit(Sender).EchoMode = emNormal);
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
      
        gtk_text_set_editable (PGtkText(ImplWidget), not (Sender as TMemo).ReadOnly);
        gtk_text_set_line_wrap(PGtkText(ImplWidget), Integer((Sender as TCustomMemo).WordWrap));
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
      end;
           
    {csImage:  MG: TImage is not a TWinControl. This code makes no sense. ToDo.
      begin
        //Image changed.
        Widget := PgtkWidget(TBitmap(Sender).Handle);
        Image := gdk_image_get(GetControlWindow(widget),0,0,
                            widget^.allocation.width,widget^.allocation.height);
        if Handle = nil 
        then TWinControl(sender).Handle := THandle(gtk_image_new(Image,nil))
        else gtk_image_set(PgtkImage(handle),Image,nil);
      end;}
      
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
  Method: TGtkObject.UpdateHint
  Params:  Sender : the lcl object which called this func 
  Returns: currently always 0

  Sets the tooltip text of the sending control.
 ------------------------------------------------------------------------------}
function TGtkObject.UpdateHint(Sender: TObject) : integer;
var
  StrTemp : PChar;
begin
  Result := 0;     // default if nobody sets it
  if Sender is TWinControl then
  with Sender as TWinControl do
  begin
    if (Length(Hint) > 0) and (ShowHint or (csDesigning in ComponentState))
    then begin
      strTemp := StrAlloc(Length(Hint) + 1);
      try
        StrPCopy(strTemp, Hint);
        // ?? TODO something with short and long hints ??
        gtk_ToolTips_Set_Tip(FGTKToolTips, PgtkWidget(Handle), StrTemp, StrTemp);
      finally
        StrDispose(strTemp);
      end;
    end 
    else gtk_ToolTips_Set_Tip(FGTKToolTips, PgtkWidget(Handle), nil, nil);
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
  AccelKey: Integer;
  AccelGroup: PGTKAccelGroup;
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
  with LCLMenuItem do
  begin
    MenuItem := PGtkWidget(Handle);
    ParentMenuWidget := PGtkWidget(Parent.Handle);

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

          AccelGroup := gtk_accel_group_new;
          gtk_menu_set_accel_group(PGtkMenu(ContainerMenu), AccelGroup);
          SetAccelGroup(ContainerMenu, AccelGroup);
        end;
      end;
      gtk_menu_insert(PGtkMenu(ContainerMenu), MenuItem, LCLMenuItem.MenuIndex);
    end;
    
    SetContainerMenuToggleSize;
    
    if GtkWidgetIsA(MenuItem,GTK_RADIO_MENU_ITEM_TYPE) then
      RegroupMenuItem(HMENU(MenuItem),GroupIndex);

    // Add accelerators
    AccelGroup := GetAccelGroup(ContainerMenu);
    AccelKey := GetAccelKey(MenuItem);
    if (AccelGroup <> nil) and (AccelKey <> 0) then begin
      gtk_accel_group_add(AccelGroup, AccelKey, GDK_MOD1_MASK,
                          GTK_ACCEL_LOCKED, PGtkObject(MenuItem),
                          'activate_item');
    end;
  end;
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
  Assert(False, Format('Trace:  [TgtkObject.IsValidDC] DC: 0x%x --> %s', [Integer(DC), BOOL_RESULT[Result]]));
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
//  Result := (GDIObject <> 0);
  if Result then
//  try
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
//  except
//    on Exception do Result := False;
//  end;
  Assert(False, Format('Trace:  [TgtkObject.IsValidGDIObject] GDIObject: 0x%x --> %s', [Integer(GDIObject), BOOL_RESULT[Result]]));
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
  Function: NewGDIObject
  Params:  none
  Returns: a gtkwinapi DeviceContext

  Creates an initial DC
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
end;

{------------------------------------------------------------------------------
  Function: SetRCFilename
  Params:  const AValue: string
  Returns: none

  Sets the gtk resource file and parses it.
 ------------------------------------------------------------------------------}
procedure TgtkObject.SetRCFilename(const AValue: string);
begin
  if FRCFilename=AValue then exit;
  FRCFilename:=AValue;
  ParseRCFile;
end;

{------------------------------------------------------------------------------
  Function: ParseRCFile
  Params:  const AValue: string
  Returns: none

  Sets the gtk resource file and parses it.
 ------------------------------------------------------------------------------}
procedure TgtkObject.ParseRCFile;
begin
  if (FRCFilename<>'') and FileExists(FRCFilename) and FRCFileParsed then
  begin
    gtk_rc_parse(PChar(FRCFilename));
    FRCFileParsed:=true;
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
  h:=PMsg(PLazQueueItem(p)^.Data)^.HWnd;
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
    if PMsg(Result^.Data)^.HWnd=HandleWnd then
      exit;
    HashItem:=HashItem^.Next;
    while (HashItem<>nil) and (HashItem^.IsOverflow) do begin
      Result:=PLazQueueItem(HashItem^.Item);
      if PMsg(Result^.Data)^.HWnd=HandleWnd then
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

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

 { =============================================================================

  $Log$
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
