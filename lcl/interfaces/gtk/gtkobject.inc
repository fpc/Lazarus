(******************************************************************************
                                   TGTKObject
 ******************************************************************************)

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
  FAccelGroup := gtk_accel_group_new();
  FTimerData  := TList.Create;
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
    ,'gdiFont', 'gdiPen', 'gdiRegion');
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
    if p^.Message=LM_PAINT then begin
      //writeln('[TgtkObject.Destroy]  freeing unused paint message ',HexStr(p^.WParam,8));
      FPaintMessages.Remove(QueueItem);
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
       ,' unremoved LM_PAINT message links left.');
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
  Method: TGtkObject.HandleEvents
  Params:  None
  Returns: Nothing

  *Note: Handle all pending messages of the GTK engine
 ------------------------------------------------------------------------------}
procedure TgtkObject.HandleEvents;
var
  Msg: TMsg; 
  p: pMsg;
begin
  //gtk_main;
  // first let gtk handle all its messages
  while gtk_events_pending<>0 do
    gtk_main_iteration_do(False);

  // then handle our own messages
  with FMessageQueue do
    while First<>nil do
    begin
      p := PMsg(First^.Data);
      Msg := p^;
      if Msg.Message=LM_PAINT then
        FPaintMessages.Remove(First);
      Delete(First);
      with Msg do
        SendMessage(hWND, Message, WParam, LParam);
      case Msg.Message of
       LM_PAINT:
         ReleaseDC(0,Msg.WParam);
      end;
      Dispose(p);
    end;
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.WaitMessage
  Params:  None
  Returns: Nothing

  *Note: Passes execution control to the GTK engine
 ------------------------------------------------------------------------------}
procedure TgtkObject.WaitMessage;
begin
  gtk_main_iteration_do(True);
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.DoEvents
  Params:  None
  Returns: Nothing

  *Note: Tells GTK Engine to process pending events
 ------------------------------------------------------------------------------}
procedure TgtkObject.DoEvents;
begin
  // dont block waiting for an event
  //gtk_main_iteration_do(False);
  gtk_main_iteration;
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.AppTerminate
  Params:  None
  Returns: Nothing

  *Note: Tells GTK Engine to halt and destroy
 ------------------------------------------------------------------------------}
procedure TGtkObject.AppTerminate;
begin
   gdk_Cursor_Destroy(Cursor_Watch);
   gdk_Cursor_Destroy(Cursor_Arrow);
   gdk_Cursor_Destroy(Cursor_Cross);
   gdk_Cursor_Destroy(Cursor_Hand1);
   gdk_Cursor_Destroy(Cursor_XTerm);
   gdk_Cursor_Destroy(Cursor_StdArrow);
   gdk_Cursor_Destroy(Cursor_HSplit);
   gdk_Cursor_Destroy(Cursor_VSplit);
   gdk_Cursor_Destroy(Cursor_SizeNWSE);
   gdk_Cursor_Destroy(Cursor_SizeNS);
   gdk_Cursor_Destroy(Cursor_SizeNESW);
   gdk_Cursor_Destroy(Cursor_SizeWE);

   gtk_object_unref(PGTKObject(FGTKToolTips));
   FGTKToolTips := nil; 
   
   DeleteObject(FStockNullBrush);
   DeleteObject(FStockBlackBrush);
   DeleteObject(FStockLtGrayBrush);
   DeleteObject(FStockGrayBrush);
   DeleteObject(FStockDkGrayBrush);
   DeleteObject(FStockWhiteBrush);
   
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
  //Attributes: TGdkWindowAttr;
  //AttributesMask: gint;
  FG,BG : TgdkColor;  //foreground and background
begin
  // initialize app level gtk engine 
  gtk_set_locale ();

  // call init and pass cmd line args
  gtk_init (@argc, @argv);

  // Create default cursor types
  Cursor_Watch := gdk_Cursor_New(gdk_Watch);
  Cursor_Arrow := gdk_Cursor_New(gdk_Arrow);
  Cursor_Cross := gdk_Cursor_New(gdk_Cross);
  Cursor_Hand1 := gdk_Cursor_New(gdk_Hand1);
  Cursor_XTerm := gdk_Cursor_New(gdk_XTerm);
  Cursor_StdArrow := gdk_Cursor_New(GDK_LEft_Ptr);
  Cursor_HSplit := gdk_Cursor_New(GDK_SB_H_DOUBLE_ARROW);
  Cursor_VSplit := gdk_Cursor_New(GDK_SB_V_DOUBLE_ARROW);
  Cursor_SizeNWSE := gdk_Cursor_New(GDK_TOP_LEFT_CORNER);
  Cursor_SizeNS := gdk_Cursor_New(GDK_SB_V_DOUBLE_ARROW);
  Cursor_SizeNESW := gdk_Cursor_New(GDK_BOTTOM_LEFT_CORNER);
  Cursor_SizeWE := gdk_Cursor_New(GDK_SB_H_DOUBLE_ARROW);

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

  // clipboard
  ClipboardTypeAtoms[ctPrimarySelection]:=GDK_SELECTION_PRIMARY;
  ClipboardTypeAtoms[ctSecondarySelection]:=GDK_SELECTION_SECONDARY;
  ClipboardTypeAtoms[ctClipboard]:=gdk_atom_intern('CLIPBOARD',0);
end;

function TgtkObject.RecreateWnd(Sender: TObject): Integer;
var
  aParent : TWinControl;
Begin
  //could we just call IntSendMessage??

  //destroy old widget
  if TWinControl(Sender).HandleAllocated then begin
    if MCaptureHandle=TWinControl(Sender).Handle then
      MCaptureHandle:=0;
    gtk_widget_destroy(PgtkWidget(TWinControl(Sender).Handle));
  end;

  aParent := TWinControl(Sender).Parent;
  aParent.RemoveControl(TControl(Sender));

  TWincontrol(Sender).Parent := nil;
  TWincontrol(Sender).Parent := aParent;

  ResizeChild(Sender,TWinControl(Sender).Left,TWinControl(Sender).Top,
     TWinControl(Sender).Width,TWinControl(Sender).Height);
  ShowHide(Sender);
  Result:=0;
End;


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
  QueueItem, OldQueueItem: PLazQueueItem; // currently only used by LM_DESTROY
  MsgPtr    : PMsg;                  // currently only used by LM_DESTROY
  Count     : Integer;               //Used in TListView LM_LV_CHANGEITEM
  Titles    : Array [0..255] of PChar;
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
         Assert(False, Format('Trace:  [TgtkObject.IntSendMessage3] %s --> Show/Hide', [Sender.ClassNAme]));
         ShowHide(Sender);
      end;

      LM_SetCursor : SetCursor(Sender);

      LM_SetLabel  : SetLabel(Sender,Data);

      LM_GETVALUE  : Result := GetValue (Sender, data);

      LM_SETVALUE  : Result := SetValue (Sender, data);
     
      LM_SETPROPERTIES: Result := SetProperties(Sender);
      
      {
      LM_SETDESIGNING
      Used in Main.pp. Used to set anything specifically needed when setting controls to Designing.
      }
      LM_SETDESIGNING : Begin  //Result := SetDesigning(sender);
                          //trying to prevent some key actions....
                          //this didn't work....
                          //if not (csAcceptsControls in TControl(Sender).ControlStyle) then gtk_widget_set_sensitive(PgtkWidget(TWinControl(sender).Handle),False);
                          if (Sender is TCustomComboBox) then
                                begin
                                gtk_combo_disable_activate(PGTKCombo(TWinControl(sender).handle));
                                end;
                        end;

      LM_RECREATEWND : Result := RecreateWnd(sender);
      
      LM_ATTACHMENU: AttachMenu(Sender);
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
//          Assert(False, Format('Trace:  [TgtkObject.IntSendMessage3] %s --> %s   ---calling INSERTBUTTON from Add Child', [AParent.ClassName, Sender.ClassNAme]));
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
      {Used by:
        TListView

      }
      LM_LV_DELETEITEM :
      begin
        if (Sender is TListView) then
        begin
          writeln('IN LM_LV_DELETEITEM');
          Num := Integer(data^);
          gtk_clist_remove(PgtkCList(handle),Num);
        end;
      end;
      LM_LV_CHANGEITEM :
      begin
        writeln('IN LM_LV_CHANGEITEM');
        if (Sender is TListView) then
        begin
          Num := Integer(data^);
          Writeln('NUM,Rows = ',num,',',PGTKCList(Handle)^.rows);

          AddItemListItem := TListView(sender).Items[Num];
          Writeln('AddItemListItem.Caption = ',AddItemListItem.Caption);
          pStr := StrAlloc(length(AddItemListItem.Caption) + 1);
          StrPCopy(pStr, AddItemListItem.Caption);
          gtk_clist_set_text(PgtkCList(handle),num,0,pStr);
          StrDispose(pStr);
          for count := 0 to AddItemListItem.SubItems.Count-1 do
             Begin
               pStr := StrAlloc(length(AddItemListItem.SubItems.Strings[Count]) + 1);
               StrPCopy(pStr, AddItemListItem.SubItems.Strings[Count]);
               gtk_clist_set_text(PgtkCList(handle),num,count+1,pStr);
               StrDispose(pStr);
             end;
        end;
      end;
      
      LM_LV_ADDITEM :
      begin
        if (Sender is TListView) then
        begin
          //get last item and add it..
          Writeln('LV_AddItem');
          pStr := StrAlloc(length('Test') + 1);
          StrPCopy(pStr, 'Test');
          Titles[0] := pStr;
          for Count := 1 to 255 do
            Titles[Count] := nil;
          Num := gtk_clist_append(PgtkCList(handle),@Titles);
          Writeln('NUm in AddItem is =',num);
          StrDispose(pStr);
          AddItemListItem := TListView(sender).Items[TListView(sender).Items.Count-1];
          if AddItemListItem <> nil then
            Begin
             gtk_clist_set_text(PgtkCList(handle),num,0,@AddItemListItem.Caption);
            end;
        Writeln('LV_AddItem DONE');
        end;
        
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
         gdk_window_raise(Widget^.Window);
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
        if (Sender is TWinControl) or (Sender is TCommonDialog) then begin
//writeln('>>> LM_DESTROY ',Sender.Classname,' Sender=',HexStr(Cardinal(Sender),8),' Handle=',HexStr(Cardinal(Handle),8));
          if Handle<>0 then begin
            if MCaptureHandle=Handle then MCaptureHandle:=0;
            if ClipboardWidget=PGtkWidget(Handle) then begin
              // clipboard widget destroyed
              if (Application<>nil)
              and (Application.MainForm<>nil)
              and (Application.MainForm.HandleAllocated)
              and (Application.MainForm.Handle<>Handle) then
                // there is still the main form left -> use it for clipboard
                SetClipboardWidget(PGtkWidget(Application.MainForm.Handle))
              else
                // program closed -> close clipboard
                SetClipboardWidget(nil);
            end;
//writeln('>>> LM_DESTROY A ',Sender.Classname,' Sender=',HexStr(Cardinal(Sender),8),' Handle=',HexStr(Cardinal(Handle),8));
{
  MG: Important:

  If Sender is TForm and one of its components was focused by SetFocus then
    the next line will produce a gtk warning. The PGtkWidget(Handle) is also
    a dirty cast. If Sender is TForm, Handle is a PGtkWindow !
}
            if gtk_type_is_a(gtk_object_type(PGtkObject(Handle)),
              GTKAPIWidget_GetType)
            then
              DestroyCaret(Handle);
            gtk_widget_destroy(PGtkWidget(Handle));
//writeln('>>> LM_DESTROY END ',Sender.Classname,' Sender=',HexStr(Cardinal(Sender),8),' Handle=',HexStr(Cardinal(Handle),8));
          end;
          // remove all remaining messages to this component
          QueueItem:=FMessageQueue.First;
          while (QueueItem<>nil) do begin
            MsgPtr := PMsg(QueueItem^.Data);
            if MsgPtr^.HWnd=Handle then begin
              // remove message
              if MsgPtr^.Message=LM_PAINT then begin
                FPaintMessages.Remove(QueueItem);
                ReleaseDC(0,MsgPtr^.WParam);
              end;
              Dispose(MsgPtr);
              OldQueueItem:=QueueItem;
              QueueItem:=QueueItem^.Next;
              FMessageQueue.Delete(OldQueueItem);
            end else
              QueueItem:=QueueItem^.Next;
          end;
        end
        else
          Assert (False, Format ('Trace:Dont know how to destroy component %s', [sender.classname]));
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
      Begin
         Assert(False, 'Trace:********************');
         Assert(False, 'Trace:1');
         box1 := gtk_object_get_data(pgtkObject(handle),'HBox');
         if box1 <> nil then
         begin
            Assert(False, 'Trace:REMOVING THE HBOX');
            gtk_container_remove(PgtkContainer(box1),gtk_object_get_data(pgtkObject(handle),'Label'));
            PixMapWid:=gtk_object_get_data(pgtkObject(handle),'Pixmap');
            if PixMapWid<>nil then
              gtk_container_remove(PgtkContainer(box1),PixMapWid);
            gtk_container_remove(PgtkContainer(handle),box1);
            // gtk_container_remove automatically destroys box1 if ref count=0
            //  so we dont need gtk_widget_destroy(box1);
         end;

         if (TBitBtn(Sender).Layout = blGlyphLeft)
         or (TBitBtn(Sender).Layout = blGlyphRight) then
         Begin
           Assert(False, 'Trace:GLYPHLEFT or GLYPHRIGHT');
           box1 := gtk_hbox_new(False,0);
         end
         else Begin
           Assert(False, 'Trace:GLYPHTOP or GLYPHBOTTOM');
           box1 := gtk_vbox_new(False,0);
         end;

         Assert(False, 'Trace:2');
         pixmap := pgdkPixmap(
                     PgdiObject(TBitBtn(Sender).Glyph.handle)^.GDIBitmapObject);
         Assert(False, 'Trace:3');
         if PgdiObject(TBitBtn(Sender).Glyph.handle)^.GDIBitmapMaskObject <> nil 
         then begin
           PixMapWid := gtk_pixmap_new(pixmap,
             PgdiObject(TBitBtn(Sender).Glyph.handle)^.GDIBitmapMaskObject)
         end else begin
           PixMapWid := gtk_pixmap_new(pixmap,nil);
         end;

         Assert(False, 'Trace:4');
         pStr := StrAlloc(length(TBitBtn(Sender).Caption) + 1);
         StrPCopy(pStr, TBitBtn(Sender).Caption);
         pLabel := gtk_label_new(pstr);
         StrDispose(pStr);
         Assert(False, 'Trace:5');
         if (TBitBtn(Sender).Layout = blGlyphLeft)
         or (TBitBtn(Sender).Layout = blGlyphTop) then
         begin
            Assert(False, 'Trace:GLYPHLEFT or GLYPHTOP');
            gtk_box_pack_start(pGTKBox(Box1),PixMapWid,False,False,
              TBitBtn(Sender).Spacing);
            gtk_box_pack_start(pGTKBox(Box1),pLabel,False,False,
              TBitBtn(Sender).Spacing);
         end
         else begin
            Assert(False, 'Trace:GLYPHRIGHT or GLYPHBOTTOM');
            gtk_box_pack_start(pGTKBox(Box1),pLabel,False,False,
              TBitBtn(Sender).Spacing);
            gtk_box_pack_start(pGTKBox(Box1),PixMapWid,False,False,
              TBitBtn(Sender).Spacing);
         end;
         Assert(False, 'Trace:6');
         gtk_object_set_data(pgtkObject(handle),'HBox',Box1);
         gtk_object_set_data(pgtkObject(handle),'Label',pLabel);
         gtk_object_set_data(pgtkObject(handle),'Pixmap',PixMapWid);
         Assert(False, 'Trace:7');
         gtk_widget_show(pixmapwid);
         gtk_widget_show(pLabel);
         gtk_container_add(PgtkContainer(handle),box1);
         gtk_widget_show(box1);
              Assert(False, 'Trace:********************');
      end;

//SH: think of TBitmap.handle!!!!
      LM_LOADXPM:
         Begin
            if (sender is TBitmap) then
            Begin
               Assert(False, 'Trace:pixmap name '+strpas(data));
               pixmap := gdk_pixmap_create_from_xpm(
                 PdeviceContext(TBitmap(sender).handle)^.drawable,
                 nil,nil,pchar(data));
               Assert(False, 'Trace:1');
               if Pixmap = nil
                  then Assert(False, 'Trace:PIXMAP NOT LOADED!');
               PdeviceContext(TBitmap(sender).handle)^.CurrentBitmap :=
                 pgdiObject(pixmap);
            end;
         end;


      LM_POPUPSHOW :
        Begin 
              gtk_menu_popup                  (PgtkMenu(TPopupMenu(Sender).Handle),
                                              nil,
                                              nil,
                                              nil,
                                              nil,
                                              0,
                                              0);	
{Displays a menu and makes it available for selection. Applications can use this
function to display context-sensitive menus, and will typically supply NULL for
the parent_menu_shell, parent_menu_item, func and data parameters.
The default menu positioning function will position the menu at the current
pointer position.
menu :	a GtkMenu.	
parent_menu_shell :	the menu shell containing the triggering menu item.	
parent_menu_item :	the menu item whose activation triggered the popup.	
func :	a user supplied function used to position the menu.	
data :	user supplied data to be passed to func.	
button :	the button which was pressed to initiate the event.	
activate_time :	the time at which the activation event occurred. 	
}

        end;

      LM_SETFILTER :
      begin 
        if Sender is TFileDialog then begin
          pStr := StrAlloc(length(TFileDialog(Sender).Filter) + 1); 
          try
            StrPCopy(pStr, TFileDialog(Sender).Filter); 
            gtk_file_selection_complete(PGtkFileSelection(Handle), pstr);
          finally
            StrDispose(pStr); 
          end;
        end;
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
//writeln('[IntSendMessage3.lm_setsize] ',PRect(Data)^.Left,',',PRect(Data)^.Top,',',
//             PRect(Data)^.Right,',',PRect(Data)^.Bottom);
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
          pStr:= StrAlloc(Length(TCommonDialog(Sender).Title) + 1);
          try
            StrPCopy(pStr, TCommonDialog(Sender).Title);
            gtk_window_set_title(PGtkWindow(handle), pStr);
          finally
             StrDispose(pStr);
          end;
        end;
        gtk_window_set_position(PGtkWindow(handle), GTK_WIN_POS_CENTER);
        if Sender is TColorDialog then
          SetColorDialogColor(Pointer(Handle),TColorDialog(Sender).Color);
          
        gtk_widget_show(PGtkWidget(handle));
        gtk_window_set_modal(PGtkWindow(handle), true);
      end;

      LM_TB_BUTTONCOUNT:
      begin
         if (Sender is TToolbar)
           then Result := pgtkToolbar(handle)^.num_Children
           else Result := -1;
      end;

//SH: think of TCanvas.handle!!!!
      LM_ReDraw : 
      begin 
        Assert(False, Format('Trace:  [TgtkObject.IntSendMessage3] %s --> Redraw', [Sender.ClassName]));
        if (Sender is TCanvas) then 
          ReDraw(PgtkWidget((Sender as TCanvas).Handle))
        else
          if not (sender is TSpeedbutton) then begin
            if Sender is TControl then
              ReDraw(PgtkWidget(Handle))
          end else
            If TSpeedbutton(sender).Visible then
              (Sender as TSpeedButton).Perform(LM_PAINT,0,0)
            else
            Begin
              Rect := TSpeedButton(sender).BoundsRect;
              InvalidateRect(TSpeedButton(sender).Parent.Handle,@Rect,True);
            end;
      end;

      LM_AddPage :
      begin
        if Sender is TControl then begin
          Assert(False, Format('Trace:  [TgtkObject.IntSendMessage3] %s --> Add NB page: %s', [Sender.ClassName, TLMNotebookEvent(Data^).Child.ClassName]));
          AddNBPage(TControl(Sender), TLMNotebookEvent(Data^).Child, 
            TLMNotebookEvent(Data^).Page);
        end;
      end;

      LM_RemovePage :
      begin
        if Sender is TControl then
          RemoveNBPage(TControl(Sender), TLMNotebookEvent(Data^).Page);
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
            if (FormIconGdiObject<>nil) and (pgtkWidget(Handle)^.Window<>nil)
            then begin
              gdk_window_set_icon(pgtkWidget(Handle)^.Window, nil, 
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
        if (Sender as TControl).fCompStyle = csCListBox
        then begin
          Widget:= GetCoreChildWidget(PGtkWidget(Handle));
          Data := TGtkCListStringList.Create(PGtkCList(Widget));
          Result := integer(Data);
        end 
        else begin
          case (Sender as TControl).fCompStyle of
            csComboBox : Widget:= PGtkCombo(Handle)^.list;
            csListBox : Widget:= GetCoreChildWidget(PGtkWidget(Handle));
          else 
            raise Exception.Create('Message LM_GETITEMS - Not implemented');
          end;
          Data:= TGtkListStringList.Create(PGtkList(Widget));
          Result:= Integer(Data);      
        end;
      end;

      LM_GETTEXT :
      begin
        Assert (true, 'WARNING:[TgtkObject.IntSendMessage3] usage of LM_GETTEXT superfluous, use interface-function GetText instead'); 
        Result := integer (nil);
      end;

      LM_GETITEMINDEX :
      begin
        case (Sender as TControl).fCompStyle of
           csListBox: 
             begin
               if Handle<>0 then begin
                 if TListBox(Sender).MultiSelect then 
                   Widget:= PGtkList(
                      GetCoreChildWidget(PGtkWidget(Handle)))^.last_focus_child
                 else begin
                   GList:= PGtkList(
                      GetCoreChildWidget(PGtkWidget(Handle)))^.selection;
                   if GList = nil 
                      then Widget:= nil
                      else Widget:= PGtkWidget(GList^.data);
                 end;  
                 if Widget = nil 
                   then Result:= -1
                   else Result:= gtk_list_child_position(
                     PGtkList(GetCoreChildWidget(PGtkWidget(Handle))), Widget);
                end else
                  Result:=-1;
             end;
           csCListBox:
             begin
               GList:= 
                 PGtkCList(GetCoreChildWidget(PGtkWidget(Handle)))^.selection;
               if GList = nil
                  then Result := -1
                  else Result := integer(GList^.Data);
             end;
           csNotebook:
             begin
               TLMNotebookEvent(Data^).Page := 
                 gtk_notebook_get_current_page(PGtkNotebook(Handle));
             end;
        end;
      end;
          
      LM_SETITEMINDEX :
      begin
        case (Sender as TControl).fCompStyle of
          csComboBox: gtk_list_select_item(PGTKLIST(PGTKCOMBO(Handle)^.list), Integer(Data));
          csListBox : gtk_list_select_item(PGtkList(GetCoreChildWidget(PGtkWidget(Handle))), Integer(Data));
          csCListBox: gtk_clist_select_row(PGtkCList(GetCoreChildWidget(PGtkWidget(Handle))), Integer(Data), 1);    // column
          csNotebook:
          begin
            Assert(False, 'Trace:Setting Page to ' + IntToStr(TLMNotebookEvent(Data^).Page));
//writeln('LM_SETITEMINDEX A ',HexStr(Cardinal(Handle),8),', ',TLMNotebookEvent(Data^).Page);
            gtk_notebook_set_page(PGtkNotebook(Handle), TLMNotebookEvent(Data^).Page);
//writeln('LM_SETITEMINDEX B ',TLMNotebookEvent(Data^).Page);
          end;
        end;
      end;
          
      LM_GETSELSTART :
      begin
         if (Sender as TControl).fCompStyle = csComboBox then
         begin
            Result:= gtk_editable_get_position(PGtkEditable(PGtkCombo(Handle)^.entry));           
         end;
      end;
          
      LM_GETSELLEN :
      begin
         if (Sender as TControl).fCompStyle = csComboBox then
         begin
            Result:= PGtkEditable(PGtkCombo(Handle)^.entry)^.selection_end_pos -
                       PGtkEditable(PGtkCombo(Handle)^.entry)^.selection_start_pos;
         end;
      end;
  
      LM_GETLIMITTEXT :
      begin
         if (Sender as TControl).fCompStyle = csComboBox then
         begin
            Result:= PGtkEntry(PGtkCombo(Handle)^.entry)^.text_max_length;
         end;
      end;
  
      LM_SETSELSTART : 
      begin
         if (Sender is TControl) and (TControl(Sender).fCompStyle = csComboBox) then
         begin
            gtk_editable_set_position(PGtkEditable(PGtkCombo(Handle)^.entry), Integer(Data));
         end;
      end;
          
      LM_SETSELLEN :
      begin
         if (Sender is TControl) and (TControl(Sender).fCompStyle = csComboBox) then
         begin
            gtk_editable_select_region(PGtkEditable(PGtkCombo(Handle)^.entry),
            gtk_editable_get_position(PGtkEditable(PGtkCombo(Handle)^.entry)),
            gtk_editable_get_position(PGtkEditable(PGtkCombo(Handle)^.entry)) + Integer(Data));
         end;
      end;  
 
      LM_GetLineCount :
      begin

      end;

      LM_GETSELCOUNT :
      begin
         case (Sender as TControl).fCompStyle of
            csListBox : Result:= g_list_length(PGtkList(GetCoreChildWidget(PGtkWidget(Handle)))^.selection);
            csCListBox: Result:= g_list_length(PGtkCList(GetCoreChildWidget(PGtkWidget(Handle)))^.selection);
         end;
      end;
  
      LM_GETSEL :
      begin
         if (Sender as TWinControl).fCompStyle = csListBox then
         begin
           { Get the child in question of that index }
           ListItem:= g_list_nth_data(PGtkList(GetCoreChildWidget(PGtkWidget(Handle)))^.children, Integer(Data^));
           Result:= g_list_index(PGtkList(GetCoreChildWidget(PGtkWidget(Handle)))^.selection, ListItem);
         end
         else if (Sender as TControl).fCompStyle = csCListBox then
         begin
            { Get the selections }
            GList:= PGtkCList(GetCoreChildWidget(PGtkWidget(Handle)))^.selection;
            Result := -1;      { assume: nothing found }
            while Assigned(GList) and (result = -1) do
            begin
               if integer(GList^.data) = integer(Data^)
                  then Result := 0
                  else GList := GList^.Next;
            end;
         end;
      end;
  
      LM_SETLIMITTEXT :
      begin
         if (Sender is TControl) and (TControl(Sender).fCompStyle = csComboBox) 
            then gtk_entry_set_max_length(PGtkEntry(PGtkCombo(Handle)^.entry), Integer(Data^));
      end;
 
      LM_SORT :
      begin
         if  (Sender is TControl) and assigned (data) then
         begin
            case TControl(Sender).fCompStyle of
               csComboBox, 
               csListBox   : TGtkListStringList(TLMSort(Data^).List).Sorted:= TLMSort(Data^).IsSorted;
               csCListBox  : TGtkCListStringList(TLMSort(Data^).List).Sorted := TLMSort(Data^).IsSorted;
            end
         end
      end;
 
      LM_SETSEL :
      begin
         if (Sender is TControl) and 
            (TControl(Sender).fCompStyle in [csListBox, csCListBox]) and
            assigned (data) then 
         begin
            if (TControl(Sender).fCompStyle = csListBox) then 
            begin
               if TLMSetSel(Data^).Selected 
                  then gtk_list_select_item(PGtkList(GetCoreChildWidget(PGtkWidget(Handle))), TLMSetSel(Data^).Index)
                  else gtk_list_unselect_item(PGtkList(GetCoreChildWidget(PGtkWidget(Handle))), TLMSetSel(Data^).Index);
            end
            else if (TControl(Sender).fCompStyle = csCListBox) then
            begin
               if TLMSetSel(Data^).Selected
                    then gtk_clist_select_row(PGtkCList(GetCoreChildWidget(PGtkWidget(Handle))),TLMSetSel(Data^).Index,0)
                  else gtk_clist_unselect_row(PGtkCList(GetCoreChildWidget(PGtkWidget(Handle))),TLMSetSel(Data^).Index,0);
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
              csListBox  : gtk_list_set_selection_mode(PGtkList(GetCoreChildWidget(PGtkWidget(Handle))), SelectionMode);
              csCListBox : gtk_clist_set_selection_mode(PGtkCList(GetCoreChildWidget(PGtkWidget(Handle))),SelectionMode);
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
                  then gtk_viewport_set_shadow_type(PGtkViewPort(Widget), GTK_SHADOW_IN)
                  else gtk_viewport_set_shadow_type(PGtkViewPort(Widget), GTK_SHADOW_NONE);
            end
            else if TControl(Sender).fCompStyle = csCListBox then
            begin
               if TListBox(Sender).BorderStyle = TBorderStyle(bsSingle)
                  then gtk_clist_set_shadow_type(
                         PGtkCList(GetCoreChildWidget(PGtkWidget(Handle))),
                         GTK_SHADOW_IN)
                  else gtk_clist_set_shadow_type(
                         PGtkCList(GetCoreChildWidget(PGtkWidget(Handle))),
                         GTK_SHADOW_NONE);
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
     csComboBox: Text := StrPas(gtk_entry_get_text(PGtkEntry(PGtkCombo((Sender as TWinControl).Handle)^.entry)));
     csEdit    : Text := StrPas(gtk_entry_get_text(PgtkEntry((Sender as TWinControl).Handle)));
     csMemo    : begin
                    CS := gtk_editable_get_chars(PGtkEditable(GetCoreChildWidget(PGtkWidget((Sender as TWinControl).Handle))), 0, -1);
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
procedure TgtkObject.ResizeChild(Sender : TObject;Left,Top,Width,Height : Integer);
var
  pFixed: PGTKFixed;
  pWidget: PGTKWidget;
  Parent: TWinControl;
begin
  Assert(false, (Format('trace:  [TgtkObject.ResizeChild] %s --> Resize', [Sender.ClassNAme])));

  Parent := TControl(Sender).Parent;

  if not (Sender is TSpeedButton) then
  begin
    pWidget := pgtkWidget(TWinControl(Sender).Handle);

    if Sender is TCustomForm then
       gtk_widget_set_usize(pWidget, -1, -1);
       
    gtk_widget_set_usize(pWidget, Width, Height);

    if Sender is TCustomForm then
       gtk_window_set_default_size(PgtkWindow(pWidget),Width,Height);

    if not ((Parent = nil) or (Sender is TCustomForm)) then
    begin
      pFixed := GetFixedWidget(PGtkWidget(Parent.Handle));
      if pFixed <> nil then
        begin
          gtk_fixed_move(pFixed, pWidget, Left, Top);
        end
      else Assert(False, 'Trace:ERROR!!!!  - no Fixed Widget found to use when resizing....');
    end
    else
     begin
      gtk_widget_set_uposition(pWidget, Left, Top);
    end;
  end;
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
  pFixed: PGTKFixed;
begin
Assert(False, 'Trace:ADDCHILD');
   pFixed := GetFixedWidget(PGtkWidget(Parent));
   if pFixed <> nil 
      then gtk_fixed_put(pFixed, Child, Left, Top);
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
  Method: TGtkObject.SetCursor
  Params:  Sender - the control which invoked this method
  Returns: Nothing

  Sets the cursor for a widget
  
  WARNING: Sender will be casted to TControl, CLEANUP!
 ------------------------------------------------------------------------------}
procedure TgtkObject.SetCursor(Sender : TObject);
//var CursorType : Integer;
begin
  Assert(False, 'Trace:IN SETCURSOR');
  If not(Sender is TWinControl) or(TWinControl(Sender).Handle = 0) then EXIT;
  Assert(False, 'Trace:IN SETCURSOR CASE STATEMENT');
  case TControl(Sender).Cursor of
     crAppStart : gdk_window_set_cursor (pgtkWidget(TWinControl(Sender).Handle)^.window, Cursor_Watch); 
     crArrow    : gdk_window_set_cursor (pgtkWidget(TWinControl(Sender).Handle)^.window, Cursor_Arrow); 
     crCross    : gdk_window_set_cursor (pgtkWidget(TWinControl(Sender).Handle)^.window, Cursor_Cross); 
     crHandPoint: gdk_window_set_cursor (pgtkWidget(TWinControl(Sender).Handle)^.window, Cursor_hand1); 
     crIBeam    : gdk_window_set_cursor (pgtkWidget(TWinControl(Sender).Handle)^.window, Cursor_XTerm); 
     crHourGlass: gdk_window_set_cursor (pgtkWidget(TWinControl(Sender).Handle)^.window, Cursor_Watch);
     crDefault  : gdk_window_set_cursor (pgtkWidget(TWinControl(Sender).Handle)^.window, Cursor_StdArrow);
     crHSplit   : gdk_window_set_cursor (pgtkWidget(TWinControl(Sender).Handle)^.window, Cursor_HSplit);
     crVSplit   : gdk_window_set_cursor (pgtkWidget(TWinControl(Sender).Handle)^.window, Cursor_VSplit);
     crSizeNWSE : gdk_window_set_cursor (pgtkWidget(TWinControl(Sender).Handle)^.window, Cursor_SizeNWSE);
     crSizeNS   : gdk_window_set_cursor (pgtkWidget(TWinControl(Sender).Handle)^.window, Cursor_SizeNS);
     crSizeNESW : gdk_window_set_cursor (pgtkWidget(TWinControl(Sender).Handle)^.window, Cursor_SizeNESW);
     crSizeWE   : gdk_window_set_cursor (pgtkWidget(TWinControl(Sender).Handle)^.window, Cursor_SizeWE);
     
  else
     Exit;
  end; 
end;


{------------------------------------------------------------------------------
  Method: TGtkObject.SetLabel
  Params:  sender - the calling object
           data   - String (PChar) to be set as label for a control
  Returns: Nothing

  Sets the label text on a widget
 ------------------------------------------------------------------------------}
procedure TgtkObject.SetLabel(Sender : TObject; Data : Pointer);
var
   P : Pointer;
   pLabel: pchar;
begin
  if Sender is TWinControl
    then Assert(False, Format('Trace:  [TgtkObject.SetLabel] %s --> label %s', [Sender.ClassName, TControl(Sender).Caption]))
    else begin
      Assert(False, Format('Trace:WARNING: [TgtkObject.SetLabel] %s --> No Decendant of TWinControl', [Sender.ClassName]));
      writeln('[TgtkObject.SetLabel] ERROR: Sender (',Sender.Classname,')'
          ,'is not TWinControl ');
      Halt;
    end;

   P := Pointer(TWinControl(Sender).Handle);
   Assert(p = nil, 'Trace:WARNING: [TgtkObject.SetLabel] --> got nil pointer');
   Assert(False, 'Trace:Setting Str1 in SetLabel');
   pLabel := pchar(Data);

   case TControl(Sender).fCompStyle of
      csBitBtn      : IntSendMessage3(LM_IMAGECHANGED,Sender,nil);

      csButton,
      csToolButton  : with PgtkButton(P)^ do
                      begin
                         if Child = nil then 
                         begin
                            Assert(False, Format('trace:  [TgtkObject.SetLabel] %s has no child label', [Sender.ClassName]));
                            child := gtk_label_new(pLabel)
                         end
                         else begin
                            Assert(False, Format('trace:  [TgtkObject.SetLabel] %s has child label', [Sender.ClassName]));
                            gtk_label_set_text(pgtkLabel(Child), PLabel);
                         end;
                      end;

      csForm, 
      csFileDialog, 
      csColorDialog,
      csFontDialog  : gtk_window_set_title(pGtkWindow(p),PLabel);

      csLabel       : gtk_label_set_text(pGtkLabel(p), pLabel);

      csCheckBox    : gtk_label_set_text(pGtkLabel( pgtkCheckButton(p)^.Toggle_Button.Button.Child),pLabel);

      csGroupBox    : gtk_frame_set_label(pgtkFrame(P),pLabel);

      csEdit        : gtk_entry_set_text(pGtkEntry(P),pLabel);

      csMemo        : begin
                        P := GetCoreChildWidget(P);
                        gtk_text_freeze(PGtkText(P));
                        gtk_text_set_point(PGtkText(P), 0);
                        gtk_text_forward_delete(
                           PGtkText(P),
                           gtk_text_get_length(PGtkText(P)));
                        gtk_text_insert(PGtkText(P), nil, nil, nil, pLabel, -1);
                        gtk_text_thaw(PGtkText(P));
                      end;      

      csPage        : gtk_notebook_set_tab_label_text(PGtkNotebook((TWinControl(Sender).Parent).handle),
                                                      PGtkWidget(P),
                                                      PGChar(data));
//GET? WHY should this be right?  p := gtk_notebook_get_tab_label(PGTKNoteBook(TWinControl(Sender).Parent.Handle), P);
      
      csComboBox    : gtk_entry_set_text(PGtkEntry(PGtkCombo(P)^.entry), PLabel);
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
{var
   TheStyle : pGtkStyle;
   widget : pgtkWidget;
   NewColor : TgdkColor;}
begin
  if Sender is TWincontrol
  then with TWincontrol(Sender) do
  begin
    // Temphack to set backcolor, till better solution
    if HandleAllocated then SetBKColor(Handle, ColorToRGB(Color));
  end;
  

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
procedure TGTKObject.SetCallback(Msg : LongInt; Sender : TObject);

  procedure ConnectSignal(const AObject:gtk_Object; const ASignal: PChar; 
    const ACallBackProc: Pointer; const ReqSignalMask: TGdkEventMask);
  var
    RealizeHandler, Handler: PGTKHandler;
    RealizeID, SignalID: guint;
  begin
    if ACallBackProc <> nil then
    begin
      // first loop though the handlers to:
      // - check if a handler already exists
      // - Find the realize handler to change data
      Handler := gtk_object_get_data_by_id (AObject, gtk_handler_quark);
      SignalID := gtk_signal_lookup(ASignal, GTK_OBJECT_TYPE(AObject));
      RealizeID := gtk_signal_lookup('realize', GTK_OBJECT_TYPE(AObject));
      RealizeHandler := nil;

      while (Handler <> nil) do
      with Handler^ do
      begin
        //look for realize handler
        if  (Id > 0) and
            (Signal_ID = RealizeID) and
            (Func = TGTKSignalFunc(@GTKRealizeCB))
           then RealizeHandler := Handler;

        if  (Id > 0) and
            (Signal_ID = SignalID) and
            (Func = TGTKSignalFunc(ACallBackProc)) and
            (func_data = Pointer(Sender))
        then begin
          Assert(False, Format('Trace:WARNING: [TGTKObject.SetCallback] %s signal <%s> set twice', [Sender.ClassName, ASignal]));
          Exit;
        end;
        Handler := Next;
      end;
      // if we are here no handler was defined yet
      Assert(False, Format('trace:  [TGTKObject.SetCallback] %s signal <%s>', [Sender.ClassName, ASignal]));
      gtk_signal_connect(AObject, ASignal, TGTKSignalFunc(ACallBackProc), Sender);
      
      if ReqSignalMask <> 0 then
      begin
        if RealizeHandler = nil 
           then
             gtk_signal_connect(AObject, 'realize', 
               TGTKSignalFunc(@GTKRealizeCB), Pointer(ReqSignalMask))
           else
             TGdkEventMask(RealizeHandler^.func_data) := 
               TGdkEventMask(RealizeHandler^.func_data) or ReqSignalMask;
      end;
    end;
  end;

  procedure ConnectSignal(const AObject:gtk_Object; const ASignal: PChar; 
    const ACallBackProc: Pointer);
  begin
    ConnectSignal(AObject, ASignal, ACallBackProc, 0);
  end;
  

var
   gObject, gFixed: PGTKObject;
begin
  gObject := ObjectToGTKObject(Sender);
  if gObject = nil then Exit;
  
  gFixed := PGTKObject(GetFixedWidget(gObject));

  if gFixed = nil then gFixed := gObject;

  case Msg of
    LM_SHOWWINDOW :
    begin
      ConnectSignal(gObject, 'show', @gtkshowCB);
      ConnectSignal(gObject, 'hide', @gtkhideCB);
    end;

    LM_DESTROY :
    begin
      ConnectSignal(gObject, 'destroy', @gtkdestroyCB);
    end;
    
    LM_CLOSEQUERY :
    begin
      ConnectSignal(gObject, 'delete-event', @gtkdeleteCB);
    end;

    LM_ACTIVATE :
    begin
      if (Sender is TCustomForm) then
         Begin
         ConnectSignal(gObject, 'focus-in-event', @gtkfrmactivate);
         ConnectSignal(gObject, 'focus-out-event', @gtkfrmdeactivate);
         end
      else
         ConnectSignal(gObject, 'activate', @gtkactivateCB);
    end;

    LM_ACTIVATEITEM :
    begin
      ConnectSignal(gObject, 'activate-item', @gtkactivateCB);
    end;

    LM_CHANGED :
       if sender is TTrackBar then begin
         ConnectSignal(gtk_Object(gtk_range_get_adjustment(
           GTK_RANGE(gObject))) , 'value_changed', @gtkvaluechanged);
       end
       else if sender is TNotebook
          then ConnectSignal(gObject, 'switch-page', @gtkswitchpage)
       else if sender is TCustomCombobox
          then ConnectSignal (PGtkObject(PGtkCombo(gobject)^.entry), 'changed', @gtkchangedCB)
       else 
            ConnectSignal(gObject, 'changed', @gtkchanged_editbox);

    LM_CLICKED :
    begin
      ConnectSignal(gObject, 'clicked', @gtkclickedCB);
    end;

    LM_CONFIGUREEVENT :
    begin
      ConnectSignal(gObject, 'configure-event', @gtkconfigureevent);
    end;
    
    LM_DAYCHANGED :  //calendar
    Begin
      ConnectSignal(gObject, 'day-selected', @gtkdaychanged);
      ConnectSignal(gObject, 'day-selected-double-click', @gtkdaychanged);

    end;

    LM_PAINT :
    begin
      ConnectSignal(gFixed, 'expose-event', @GTKExposeEvent);
      ConnectSignal(gFixed, 'draw', @GTKDraw);
    end;

    LM_EXPOSEEVENT :
    begin
//      ConnectSignal(gFixed, 'expose-event', @gtkexposeevent)
    end;

    LM_FOCUS :
    begin
      //ConnectSignal(gObject, 'focus', @gtkfocusCB);
      if (sender is TCustomComboBox) then
         Begin
          ConnectSignal(PgtkObject(PgtkCombo(TCustomComboBox(sender).handle)^.entry), 'focus-in-event', @gtkFocusCB);
          ConnectSignal(PgtkObject(PgtkCombo(TCustomComboBox(sender).handle)^.entry), 'focus-out-event', @gtkKillFocusCB);
          ConnectSignal(PgtkObject(PgtkCombo(TCustomComboBox(sender).handle)^.list), 'focus-in-event', @gtkFocusCB);
          ConnectSignal(PgtkObject(PgtkCombo(TCustomComboBox(sender).handle)^.list), 'focus-out-event', @gtkKillFocusCB);
         end
         else
         Begin
          ConnectSignal(gObject, 'focus-in-event', @gtkFocusCB);
          ConnectSignal(gObject, 'focus-out-event', @gtkKillFocusCB);
         end;
    end;

    LM_KEYDOWN,
    LM_CHAR,
    LM_KEYUP,
    LM_SYSKEYDOWN,
    LM_SYSCHAR,
    LM_SYSKEYUP:
    begin
      if (sender is TComboBox) then Begin
        ConnectSignal(PgtkObject(PgtkCombo(TComboBox(sender).handle)^.entry), 'key-press-event', @GTKKeyUpDown, GDK_KEY_PRESS_MASK);
        ConnectSignal(PgtkObject(PgtkCombo(TComboBox(sender).handle)^.entry), 'key-release-event', @GTKKeyUpDown, GDK_KEY_RELEASE_MASK);
      end
      else
      if (sender is TCustomForm) then Begin
        ConnectSignal(PgtkObject(TCustomForm(sender).handle), 'key-press-event', @GTKKeyUpDown, GDK_KEY_PRESS_MASK);
        ConnectSignal(PgtkObject(TCustomForm(sender).handle), 'key-release-event', @GTKKeyUpDown, GDK_KEY_RELEASE_MASK);
      end
      else
      if (sender is TCustomListBox) then
        Begin
        //TODO:listbox is STILL not sendig keypress events even wtih these lines.
        ConnectSignal(PgtkObject(GetCoreChildWidget(PgtkWidget(TCustomListBox(sender).handle))), 'key-press-event', @GTKKeyUpDown, GDK_KEY_PRESS_MASK);
        ConnectSignal(PgtkObject(GetCoreChildWidget(PgtkWidget(TCustomListBox(sender).handle))), 'key-release-event', @GTKKeyUpDown, GDK_KEY_RELEASE_MASK);
        end;
      ConnectSignal(gFixed, 'key-press-event', @GTKKeyUpDown, GDK_KEY_PRESS_MASK);
      ConnectSignal(gFixed, 'key-release-event', @GTKKeyUpDown, GDK_KEY_RELEASE_MASK);
    end;
    
    LM_MONTHCHANGED :  //calendar
    Begin
      ConnectSignal(gObject, 'month-changed', @gtkmonthchanged);
      ConnectSignal(gObject, 'prev-month', @gtkmonthchanged);
      ConnectSignal(gObject, 'next-month', @gtkmonthchanged);
    end;

    LM_MOUSEMOVE:
    begin
      if (sender is TComboBox) then Begin
        ConnectSignal(PgtkObject(PgtkCombo(TComboBox(sender).handle)^.entry), 'motion-notify-event', @GTKMotionNotify, GDK_POINTER_MOTION_MASK);
        ConnectSignal(PgtkObject(PgtkCombo(TComboBox(sender).handle)^.button), 'motion-notify-event', @GTKMotionNotify, GDK_POINTER_MOTION_MASK);
      end
       else
       ConnectSignal(gFixed, 'motion-notify-event', @GTKMotionNotify, GDK_POINTER_MOTION_MASK)
    end;
    
    LM_PRESSED :
    begin
      Assert(False, 'Trace:OBSOLETE: [TGTKObject.SetCallback] LM_PRESSED');
      ConnectSignal(gObject, 'pressed', @gtkpressedCB);
    end;

    LM_RELEASED :
    begin
      Assert(False, 'Trace:OBSOLETE: [TGTKObject.SetCallback] LM_RELEASED');
      ConnectSignal(gObject, 'released', @gtkreleasedCB);
    end;

    LM_MOVECURSOR :
    begin
      ConnectSignal(gObject, 'move-cursor', @gtkmovecursorCB);
    end;

    LM_LBUTTONDOWN,
    LM_RBUTTONDOWN,
    LM_MBUTTONDOWN,
    LM_MOUSEWHEEL :
    begin
      if (sender is TCustomComboBox) then
         Begin
           ConnectSignal(PgtkObject(PgtkCOmbo(gObject)^.entry), 'button-press-event',  @gtkmousebtnpress, GDK_BUTTON_PRESS_MASK);
           ConnectSignal(PgtkObject(PgtkCOmbo(gObject)^.button) , 'button-press-event', @gtkmousebtnpress, GDK_BUTTON_PRESS_MASK);
//         Connecting the list seems to cause errors. Maybe we are returning the wrong boolean in the callback func
//           ConnectSignal(PgtkObject(PgtkCOmbo(gObject)^.list), 'button-press-event',  @gtkmousebtnpress, GDK_BUTTON_PRESS_MASK);

         end
         else
           ConnectSignal(gFixed, 'button-press-event', @gtkmousebtnpress, GDK_BUTTON_PRESS_MASK);
    end;

    LM_LBUTTONUP,
    LM_RBUTTONUP,
    LM_MBUTTONUP:
    begin
      if (sender is TCustomComboBox) then
         Begin
           ConnectSignal(PgtkObject(PgtkCOmbo(gObject)^.entry), 'button-release-event',  @gtkmousebtnrelease, GDK_BUTTON_RELEASE_MASK);
           ConnectSignal(PgtkObject(PgtkCOmbo(gObject)^.button) , 'button-release-event', @gtkmousebtnrelease, GDK_BUTTON_RELEASE_MASK);
//         Connecting the list seems to cause errors. Maybe we are returning the wrong boolean in the callback func
//           ConnectSignal(PgtkObject(PgtkCOmbo(gObject)^.list), 'button-release-event',  @gtkmousebtnrelease, GDK_BUTTON_RELEASE_MASK);

         end
         else
      ConnectSignal(gFixed, 'button-release-event', @gtkmousebtnrelease, GDK_BUTTON_RELEASE_MASK);
    end;

    LM_ENTER :
    begin
      if sender is TButton 
      then ConnectSignal(gObject, 'enter', @gtkenterCB)
      else ConnectSignal(gObject, 'focus-in-event', @gtkFocusInNotifyCB); //TODO: check this focus in is mapped to focus
    end;

    LM_EXIT :
    begin
      if sender is TButton 
      then ConnectSignal(gObject, 'leave', @gtkleaveCB)
      else ConnectSignal(gObject, 'focus-out-event', @gtkFocusOutNotifyCB);
    end;

    LM_LEAVE :
    begin
      ConnectSignal(gObject, 'leave', @gtkleaveCB);
    end;

    LM_WINDOWPOSCHANGED: //LM_SIZEALLOCATE, LM_RESIZE :
    begin
      ConnectSignal(gObject, 'size-allocate', @gtksize_allocateCB);
    end;

    LM_CHECKRESIZE :
    begin
      ConnectSignal(gObject, 'check-resize', @gtkresizeCB);
    end;

    LM_INSERTTEXT :
    begin
      ConnectSignal(gObject, 'insert-text', @gtkinserttext);
    end;

    LM_DELETETEXT :
    begin
      ConnectSignal(gObject, 'delete-text', @gtkdeletetext);
    end;

    LM_SETEDITABLE :
    begin
      ConnectSignal(gObject, 'set-editable', @gtkseteditable);
    end;

    LM_MOVEWORD :
    begin
      ConnectSignal(gObject, 'move-word', @gtkmoveword);
    end;

    LM_MOVEPAGE :
    begin
      ConnectSignal(gObject, 'move-page', @gtkmovepage);
    end;

    LM_MOVETOROW :
    begin
      ConnectSignal(gObject, 'move-to-row', @gtkmovetorow);
    end;

    LM_MOVETOCOLUMN :
    begin
      ConnectSignal(gObject, 'move-to-column', @gtkmovetocolumn);
    end;

    LM_KILLCHAR :
    begin
      ConnectSignal(gObject, 'kill-char', @gtkkillchar);
    end;

    LM_KILLWORD :
    begin
      ConnectSignal(gObject, 'kill-word', @gtkkillword);
    end;

    LM_KILLLINE :
    begin
      ConnectSignal(gObject, 'kill-line', @gtkkillline);
    end;

    LM_CUTTOCLIP :
    begin
      ConnectSignal(gObject, 'cut-clipboard', @gtkcuttoclip);
    end;

    LM_COPYTOCLIP :
    begin
      ConnectSignal(gObject, 'copy-clipboard', @gtkcopytoclip);
    end;

    LM_PASTEFROMCLIP :
    begin
      ConnectSignal(gObject, 'paste-clipboard', @gtkpastefromclip);
    end;

    LM_HSCROLL:
    begin
      ConnectSignal(PGTKObject(gtk_scrolled_window_get_hadjustment(PGTKScrolledWindow(gObject))), 'value-changed', @GTKHScrollCB);
    end;

    LM_VSCROLL:
    begin
      ConnectSignal(PGTKObject(gtk_scrolled_window_get_vadjustment(PGTKScrolledWindow(gObject))), 'value-changed', @GTKVScrollCB);
    end;
    
    LM_YEARCHANGED :  //calendar
    Begin
      ConnectSignal(gObject, 'prev-year', @gtkyearchanged);
      ConnectSignal(gObject, 'next-year', @gtkyearchanged);
    end;

(*
    LM_WINDOWPOSCHANGED:
    begin
      ConnectSignal(gObject, 'size-allocate', @gtkSizeAllocateCB);
//      ConnectSignal(gObject, 'move_resize', @gtkmoveresize);
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
  Info: PWinWidgetInfo;
  Widget: pointer;
begin
  gObject := ObjectToGTKObject(Sender);
  if gObject = nil then Exit;

  if (Sender is TWinControl) and (TWinControl(Sender).HandleAllocated) then
  begin
    Widget:=Pointer(TWinControl(Sender).Handle);
    Info := GetWidgetInfo(Widget, False);
    if Info <> nil then Dispose(Info);
    gtk_object_set_data(Widget, 'widgetinfo', nil);
  end;
  gtk_signal_handlers_destroy(gObject);
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
    GTK_WINDOW_DIALOG, GTK_WINDOW_TOPLEVEL, GTK_WINDOW_TOPLEVEL,
    GTK_WINDOW_TOPLEVEL, GTK_WINDOW_POPUP, GTK_WINDOW_POPUP
  );
  FormSizeableMap : array[TFormBorderStyle] of gint = (0, 0, 1, 0, 0, 1);
//unused:  FormBorderWidth : array[TFormBorderStyle] of gint = (0, 1, 2, 1, 1, 2);  

//unused:type
//unused:   Tpixdata = Array[1..20] of String;

var
   caption : ansistring;          // the caption of "Sender"
   StrTemp : PChar;               // same as "caption" but as PChar
   TempWidget : PGTKWidget;       // pointer to gtk-widget (local use when neccessary)
   p          : pointer;          // ptr to the newly created GtkWidget
   CompStyle,                     // componentstyle (type) of GtkWidget which will be created
   TempInt   : Integer;           // local use when neccessary
   Adjustment: PGTKAdjustment;    // currently only used for csFixed
   // - for csBitBtn
   Box       : Pointer;           // currently only used for TBitBtn and TForm
   //pixmap    : pGdkPixMap;        // TBitBtn - the default pixmap
   pixmapwid : pGtkWidget;        // currently only used for TBitBtn
   //mask      : pGDKBitmap;        // currently only used for TBitBtn
   //style     : pgtkStyle;         // currently only used for TBitBtn
   label1    : pgtkwidget;        // currently only used for TBitBtn
   //TempStr   : String;            // currently only used for TBitBtn to load default pixmap
   //pStr      : PChar;             // currently only used for TBitBtn to load default pixmap

begin
   Assert(False, 'Trace:In CreateComponet');
   p := nil;

   CompStyle := csNone;
   Caption   := Sender.ClassName;
   if (Sender is TControl) then
   begin
      caption := TControl(Sender).caption;
      CompStyle := TControl(Sender).FCompStyle
   end
   else if (Sender is TMenuItem) then
   begin
      caption := TMenuItem(Sender).caption;
      CompStyle := TMenuItem(Sender).FCompStyle;
   end
   else if (Sender is TMenu) or (Sender is TPopupMenu)
      then CompStyle := TMenu(Sender).FCompStyle
   else if (Sender is TCommonDialog)
      then CompStyle := TCommonDialog(Sender).FCompStyle
   else
      ;

   // ToDo: the following is for debug only
   if caption = '' then caption := Sender.ClassName;
   Assert(False, 'Trace:----------------------Creating component in TgtkObject-   STR = '+caption+'-');
   // ToDo: until here remove when debug not needed

   if caption = '' then caption := 'Blank';

   strTemp := StrAlloc(length(caption) + 1);
   StrPCopy(strTemp, caption);

   Assert(False, 'Trace:1');
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
        Assert(False, 'Trace:CSBITBTN CREATE*************************');
        p := gtk_button_new;
        if (TBitBtn(Sender).Layout = blGlyphLeft)
        or (TBitBtn(Sender).Layout = blGlyphRight) then begin
          Assert(False, 'Trace:GLYPHLEFT or GLYPHRIGHT');
          Box := gtk_hbox_new(False,0);
        end
        else Begin
          Assert(False, 'Trace:GLYPHTOP or GLYPHBOTTOM');
          Box := gtk_vbox_new(False,0);
        end;
        gtk_container_set_border_width(PgtkContainer(Box),2);
        //style := gtk_widget_get_style(pGTKWidget(p));
        {
             // is this neccessary?
             // MWE: nope, if needed, it should be done static
             TempStr := './images/menu.xpm';
             pStr := StrAlloc(length(TempStr) + 1);
             StrPCopy(pStr, TempStr);
             pixmap := gdk_pixmap_create_from_xpm(pgtkWidget(p)^.window, @Mask,
                                   @style^.bg[GTK_STATE_NORMAL],pStr);
             StrDispose(pStr);
        }
      
        //pixmapwid := gtk_pixmap_new(pixmap,mask);
        PixMapWid := nil;
        Label1 := gtk_label_new(StrTemp);

        //gtk_box_pack_start(pGTkBox(Box),pixmapwid,False,False,3);
        gtk_box_pack_start(pGTkBox(Box), Label1, FALSE, FALSE, 3);
        //gtk_widget_show(pixmapwid);
        gtk_widget_show(Label1);
        gtk_Container_add(PgtkContainer(p),Box);
        gtk_widget_show(Box);

        gtk_object_set_data(pgtkObject(p),'HBox',Box);
        gtk_object_set_data(pgtkObject(p),'Pixmap',PixMapwid);
        gtk_object_set_data(pgtkObject(p),'Label',Label1);

        Assert(False, 'Trace:CSBITBTN CREATE EXITING*************************');
      end;

      csButton :
      begin
         p := gtk_button_new_with_label(StrTemp);
      end;
      
      csCalendar :
      begin
         p := gtk_calendar_new();
         
      end;

      csCheckbox :
      begin
         p := gtk_check_button_new_with_label(strTemp);
      end;

      csComboBox :
      begin
         p := gtk_combo_new();
         gtk_entry_set_text(PGtkEntry(PGtkCombo(p)^.entry), StrTemp);
      end;
            
      csListBox :
      begin
         p:= gtk_scrolled_window_new(nil, nil);
         GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.hscrollbar, GTK_CAN_FOCUS);
         GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.vscrollbar, GTK_CAN_FOCUS);
         gtk_scrolled_window_set_policy(PGtkScrolledWindow(p), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
         gtk_widget_show(p);
         TempWidget:= gtk_list_new;
         gtk_scrolled_window_add_with_viewport(PGtkScrolledWindow(p), TempWidget);
               gtk_container_set_focus_vadjustment(PGtkContainer(TempWidget), 
         gtk_scrolled_window_get_vadjustment(PGtkScrolledWindow(p)));
         gtk_container_set_focus_hadjustment(PGtkContainer(TempWidget),
         gtk_scrolled_window_get_hadjustment(PGtkScrolledWindow(p)));
         gtk_widget_show(TempWidget);
         
         //--------------------------
         // MWE: will be obsoleted
         SetCoreChildWidget(p, TempWidget);
         //--------------------------
         GetWidgetInfo(p, True)^.ImplementationWidget := TempWidget;
         SetMainWidget(p, TempWidget);
      end;

      csCListBox :
      begin
         Assert(False, 'Trace:!!!!!!!!!!!!!!!! Creating Clist box !!!!!!!!!!!!!!');
         p:= gtk_scrolled_window_new(nil, nil);
         GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.hscrollbar,
                                GTK_CAN_FOCUS);
         GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.vscrollbar,
                                GTK_CAN_FOCUS);
         gtk_scrolled_window_set_policy(PGtkScrolledWindow(p),
                                        GTK_POLICY_AUTOMATIC,
                                        GTK_POLICY_AUTOMATIC);
         gtk_widget_show(p);

         with Sender as TCListBox
         do begin
           TempWidget:= gtk_clist_new(ListColumns);
           gtk_container_add(PGtkContainer(p), TempWidget);

           for TempInt := 0 to ListColumns - 1 do 
             gtk_clist_set_column_width(PGtkCList(TempWidget), TempInt, (Width-50) div ListColumns);
         end;
         gtk_widget_show(TempWidget);

         //--------------------------
         // MWE: will be obsoleted
         SetCoreChildWidget(p, TempWidget);
         //--------------------------
         GetWidgetInfo(p, True)^.ImplementationWidget := TempWidget;
         SetMainWidget(p, TempWidget);
      end;
      
      csListView :
      Begin
         P := PgtkWidget(gtk_clist_new(TCustomListView(sender).Columns.Count));
         gtk_widget_show(P);
      end;
      

      csEdit :
      begin
         p :=  gtk_entry_new();
      end;
      
      csFileDialog :
      begin
         P := gtk_file_selection_new(StrTemp);

         {****This is a major hack put by Cliff Baeseman to solve
          a gtk win32 dll implementation problem where the headers implementation
          does not match the linux version**** }
         {$ifdef LINUX}
            gtk_signal_connect( gtk_object((PGtkFileSelection(P))^.ok_button), 'clicked', gtk_signal_func(@gtkDialogOKclickedCB), Sender);
            gtk_signal_connect( gtk_object((PGtkFileSelection(P))^.cancel_button), 'clicked', gtk_signal_func(@gtkDialogCancelclickedCB), Sender);
         {$endif}

         {$ifdef WIN32}
            gtk_signal_connect( gtk_object((PGtkFileSelection(P))^.cancel_button), 'clicked', gtk_signal_func(@gtkDialogOKclickedCB), Sender);
            gtk_signal_connect( gtk_object((PGtkFileSelection(P))^.help_button), 'clicked', gtk_signal_func(@gtkDialogCancelclickedCB), Sender);
         {$endif}
         gtk_signal_connect( gtk_object(P), 'destroy', gtk_Signal_Func(@gtkDialogDestroyCB), Sender);
         
         if Sender is TOpenDialog then begin
           // set extra options
           if ofAllowMultiSelect in TOpenDialog(Sender).Options then
	   begin
	     LastFileSelectRow := -1;
             gtk_signal_connect( gtk_object(PGtkCList(PGtkFileSelection(P)^.file_list)), 'select-row', gtk_signal_func(@gtkOpenDialogRowSelectCB), Sender);
             gtk_clist_set_selection_mode(
PGtkCList(PGtkFileSelection(P)^.file_list),GTK_SELECTION_MULTIPLE);
           end;
         end;
      end;

      csColorDialog :
      begin
        P := gtk_color_selection_dialog_new(StrTemp);
        // We will only add this line if we see problem in the future with the color dialog  MAH 7-31-99
        // gtk_color_selection_set_update_policy(GTK_COLOR_SELECTION((GTK_COLOR_SELECTION_DIALOG(P))^.colorsel), GTK_UPDATE_DISCONTINUOUS);
        gtk_signal_connect( gtk_object((GTK_COLOR_SELECTION_DIALOG(P))^.ok_button), 'clicked', gtk_signal_func(@gtkDialogOKclickedCB), Sender);
        gtk_signal_connect( gtk_object((GTK_COLOR_SELECTION_DIALOG(P))^.cancel_button), 'clicked', gtk_signal_func(@gtkDialogCancelclickedCB), Sender);
        gtk_signal_connect( gtk_object(P), 'destroy', gtk_Signal_Func(@gtkDialogDestroyCB), Sender);
      end;

      csFontDialog :
      begin
         P := gtk_Font_selection_dialog_new(StrTemp);
         gtk_signal_connect( gtk_object((GTK_FONT_SELECTION_DIALOG(P))^.ok_button), 'clicked', gtk_signal_func(@gtkDialogOKclickedCB), Sender);
         gtk_signal_connect( gtk_object((GTK_FONT_SELECTION_DIALOG(P))^.cancel_button), 'clicked', gtk_signal_func(@gtkDialogCancelclickedCB), Sender);
         gtk_signal_connect( gtk_object(P), 'destroy', gtk_Signal_Func(@gtkDialogDestroyCB), Sender);
      end;

      csFixed: //used for TWinControl, maybe change this to csWinControl 
      begin
      writeln('[TgtkObject.CreateComponent] csFixed A  ',Sender.Classname);
        p := GTKAPIWidget_New;
      writeln('[TgtkObject.CreateComponent] csFixed B');
        gtk_scrolled_window_set_policy(PGTKScrolledWindow(p), GTK_POLICY_NEVER, GTK_POLICY_NEVER);

        Adjustment := gtk_scrolled_window_get_vadjustment(PGTKScrolledWindow(p));
        if Adjustment <> nil 
        then with Adjustment^ do 
        begin
          gtk_object_set_data(PGTKObject(Adjustment), 'ScrollBar', PGTKScrolledWindow(p)^.VScrollBar);
          Step_Increment := 1; 
        end;
        Adjustment := gtk_scrolled_window_get_hadjustment(PGTKScrolledWindow(p));
        if Adjustment <> nil 
        then with Adjustment^ do 
        begin
          gtk_object_set_data(PGTKObject(Adjustment), 'ScrollBar', PGTKScrolledWindow(p)^.HScrollBar);
          Step_Increment := 1; 
        end;
      end;
      
      csForm :
      begin
         Assert(Sender is TForm);
         p := gtk_window_new(FormStyleMap[TForm(Sender).BorderStyle]);
// I comment the next line because this causes that when I align something
// it's right and bottom don't visible.
//Nagy Zsolt 2001/03/27
//         gtk_container_set_border_width(GTK_CONTAINER(P), 2);
         //TempInt:= FormSizeableMap[TForm(Sender).BorderStyle];
         //gtk_window_set_policy (GTK_WINDOW (p), TempInt, TempInt, 0);
         gtk_window_set_policy (GTK_WINDOW (p), 1, 1, 0);
         gtk_window_set_title(pGtkWindow(p), strTemp);
        
         // the clipboard needs a widget
         if ClipboardWidget=nil then
           SetClipboardWidget(p);
        
         // Create the VBox, we need that to place controls outside 
         // the client area (like menu)
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
      
      csHintWindow :
      Begin
         p := gtk_window_new(FormStyleMap[bsToolWindow]{gtk_window_Popup});
         gtk_window_set_policy (GTK_WINDOW (p), 0, 0, 0);
//         Box := gtk_vbox_new(False, 0);
//         gtk_container_add(p, Box);
//         gtk_widget_show(Box);

         // Create the form client area
         TempWidget := gtk_fixed_new();
         gtk_container_add(p, TempWidget);//         gtk_box_pack_end(Box, TempWidget, True, True, 0);
         gtk_widget_show(TempWidget);
         SetFixedWidget(p, TempWidget);
         SetMainWidget(p, TempWidget);
         gtk_widget_show(p);
      end;

      csLabel :
      begin
         P := gtk_label_new(StrTemp);
         gtk_misc_set_alignment(PGTKMISC(P), 0.0 , 1.0);
      end;

      csMemo :
      begin
        P := gtk_scrolled_window_new(nil, nil);
        TempWidget := gtk_text_new(nil, nil);
        gtk_text_set_editable (PGtkText(TempWidget), not (Sender as TMemo).ReadOnly);
        gtk_text_set_word_wrap(PGtkText(TempWidget), Integer((Sender as TCustomMemo).WordWrap));
        gtk_widget_show(TempWidget);
        
        gtk_container_add(p, TempWidget); 
        case (Sender as TCustomMemo).Scrollbars of 
          ssHorizontal: gtk_scrolled_window_set_policy(p, GTK_POLICY_ALWAYS, GTK_POLICY_NEVER);
          ssVertical:   gtk_scrolled_window_set_policy(p, GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
          ssBoth:       gtk_scrolled_window_set_policy(p, GTK_POLICY_ALWAYS, GTK_POLICY_ALWAYS);
        else
          gtk_scrolled_window_set_policy(p, GTK_POLICY_NEVER, GTK_POLICY_NEVER);
        end;

        //--------------------------
        // MWE: will be obsoleted
        SetCoreChildWidget(p, TempWidget);
        //--------------------------
        GetWidgetInfo(p, True)^.ImplementationWidget := TempWidget;
        
(*         // Assert(False, 'Trace:Creating a MEMO...');
         P := gtk_hbox_new(false, 0);
         TempWidget := gtk_text_new(nil,nil);
         gtk_text_set_editable (PGtkText(TempWidget), not (Sender as TMemo).ReadOnly);
         gtk_text_set_word_wrap(PGtkText(TempWidget), Integer((Sender as TCustomMemo).WordWrap));
         gtk_box_pack_start(PGtkBox(P), TempWidget, true, true, 0);
         gtk_widget_show(TempWidget);

         //--------------------------
         // MWE: will be obsoleted
         SetCoreChildWidget(p, TempWidget);
         //--------------------------
         GetWidgetInfo(p, True)^.ImplementationWidget := TempWidget;
         SetMainWidget(p, TempWidget);

         if (Sender as TCustomMemo).Scrollbars in [ssVertical, ssBoth]
         then begin
           TempWidget := gtk_vscrollbar_new(PGtkText(TempWidget)^.vadj);
           gtk_box_pack_start(PGtkBox(P), TempWidget, false, false, 0);
           gtk_widget_show(TempWidget);
           SetMainWidget(p, TempWidget);
         end;
{
         if (Sender as TCustomMemo).Scrollbars in [ssHorizontal, ssBoth]
         then begin
           TempWidget := gtk_hscrollbar_new(PGtkText(TempWidget)^.hadj);
           gtk_box_pack_start(PGtkBox(P), TempWidget, false, false, 0);
           gtk_widget_show(TempWidget);
           SetMainWidget(p, TempWidget);
         end;
}
*)
         gtk_widget_show(P);
      end;
      
      csMainMenu:
      begin
         p := gtk_menu_bar_new();
         // get the VBox, the form has one child, a VBox
         Box := PGTKBin(TWinControl(TMenu(Sender).Owner).Handle)^.Child;
         gtk_box_pack_start(Box, p, False, False, 0);
         SetAccelGroup(p, gtk_accel_group_get_default);
         gtk_widget_show(p);
      end;

      csMenuBar :
      begin
         P := gtk_menu_bar_new();
         gtk_container_add(GTK_Container(GetFixedWidget(Pointer(TWinControl(TMenu(Sender).Owner).Handle))), P);
         SetAccelGroup(p, gtk_accel_group_get_default);
         gtk_widget_show(p);
      end;

      csMenuItem :
      begin
        if Caption <> '-' 
        then begin
          //Check for an shortcut key
          tempInt := pos('&', Caption);
          if tempInt <> 0 then
          begin
             StrTemp[tempInt - 1] := '_'; 
             P := gtk_menu_item_new_with_label('');
             SetAccelKey(P, gtk_label_parse_uline(PGTKLabel(PGTKBin(p)^.Child), StrTemp));
          end
          else P := gtk_menu_item_new_with_label(Strtemp)
        end
        else P := gtk_menu_item_new;

        gtk_widget_show (p);
      end;

      csNotebook :
      begin
         P := gtk_notebook_new();
         gtk_notebook_set_scrollable(P, true);
         gtk_notebook_popup_enable(P);
         gtk_notebook_set_show_tabs(P, false);   // Turn tabs off

         {
         MWE: The FixedWidged pops up as an Page
         We don't want that. BTW Adding controls to the notebookcontrol ???
   }  
{         TempWidget := gtk_fixed_new();
         gtk_container_add(GTK_CONTAINER(p), TempWidget);
         gtk_widget_show(TempWidget);
         gtk_Object_Set_Data(Pgtkobject(p),'Fixed',tempWidget);
         gtk_object_set_data(PGTKObject(TempWidget), 'Main', p);
   
         gtk_Object_Set_Data(Pgtkobject(tempwidget),'Owner',p);

         ??? Fixed object_data not only a container ???
         Without, the notebook dumps
         This should be fixed someday 
         }
//         Assert(False, 'Trace:FIXME !!! [TgtkObject.CreateComponent] csNotebook --> gtk_Object_Set_Data');
//         SetFixedWidget(p, p);

      end;

      csRadioButton :
      with sender as TRadioButton do
      begin
         if group = 0 then
         begin
            P := gtk_radio_button_new_with_label(PGsList(group),StrTemp);
            group := THandle (gtk_radio_button_group (GTk_Radio_Button(P)));
         end
         else begin
            P := gtk_radio_button_new_with_label(gtk_radio_button_group (GTk_Radio_Button(group)),StrTemp);
         end;
      end;

      csScrollBar :
      begin
            if (TScrollBar(sender).kind = sbHorizontal) then
               begin
                 P := gtk_hscrollbar_new(PgtkAdjustment(gtk_adjustment_new(1,TScrollBar(sender).min, TScrollBar(sender).max,
                                                                     TScrollBar(sender).SmallChange, TScrollBar(sender).LargeChange,
                                                                     TScrollBar(sender).Pagesize)));
               end
              else
               Begin
                 P := gtk_vscrollbar_new(PgtkAdjustment(gtk_adjustment_new(1,TScrollBar(sender).min, TScrollBar(sender).max,
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
       {p := gtk_drawing_area_new();
            gtk_drawing_area_size(pGTKDrawingArea(p),22,22);}
       //nothing done here.  We are only worried about the canvas
      end;

      csSpinEdit :
      begin
        p := gtk_spin_button_new(PgtkAdjustment(gtk_adjustment_new(1,1,100,1,1,1)),1,0);
      end;

      csSTATUSBAR :
      begin
         P := gtk_statusbar_new();
         Assert(False, 'Trace:In CreateComponent --StatusBar');
      end;

      csgtkTable  :
      begin
         P := gtk_table_new(2,2,False);
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
         if TToolButton(Sender).Style = tbsButton then
         Begin 
            p := gtk_button_new_with_label(StrTemp);
            Assert(False, 'Trace:TTOOLBUTTON created as type TBSBUTTON');
         end
         else Begin
            p := gtk_button_new_with_label(StrTemp);
            Assert(False, 'Trace:TTOOLBUTTON created as type TBSBUTTON because type was unknown');
         end;
         gtk_widget_show (P);
{        p := gtk_toolbar_prepend_item(pGTKToolbar(TWinControl(TWincontrol(sender).Parent).Handle),
              str,Str2,nil,nil,gtk_signal_func(@gtkclickedCB),Sender);
}
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

      csPage:    // TPage - Notebook page 
      begin
         P := gtk_hbox_new(false, 0);
         TempWidget := gtk_fixed_new();
         gtk_container_add(GTK_CONTAINER(P), TempWidget);
         gtk_widget_show(TempWidget);
         SetFixedWidget(p, TempWidget);
         SetMainWidget(p, TempWidget);
         gtk_widget_show (P);
      end;

      csPopupMenu :
      with (TPopupMenu(Sender)) do
          Begin
          P := gtk_menu_new();

          end;
          
      csProgressBar:
      with (TProgressBar (Sender)) do
      begin
         { Create a GtkAdjusment object to hold the range of the progress bar }
         TempWidget := PGtkWidget( gtk_adjustment_new (Position, Min, Max, 0, 0, 0));
         { Create the GtkProgressBar using the adjustment }
         P := gtk_progress_bar_new_with_adjustment (PGtkAdjustment (TempWidget));
      end;

      csTrackBar:
      with (TTrackBar (Sender)) do
      begin
         TempWidget := PGtkWidget( gtk_adjustment_new (Position, Min, Max, linesize, pagesize, 0));
         if (Orientation = trHorizontal) 
           then P := gtk_hscale_new (PGTKADJUSTMENT (TempWidget))
           else P := gtk_vscale_new (PGTKADJUSTMENT (TempWidget));
         gtk_scale_set_digits (PGTKSCALE (P), 0);
      end;   
      
   end; //case


   if (Sender is TWinControl) then 
   begin
     TWinControl(Sender).Handle := THandle(p);
     if p <> nil then
       gtk_object_set_data(pgtkobject(p),'Sender',Sender);
   end
   else if (Sender is TMenuItem)
     then TMenuItem(Sender).Handle := HMenu(p)
   else if (Sender is TMenu)
     then TMenu(Sender).Items.Handle := HMenu(p)
   else if (Sender is TCommonDialog)
     then TCommonDialog(Sender).Handle:= THandle(p);

// MWE: next will be obsoleted by WinWidgetInfo
//Set these for functions like GetWindowLong  Added 01/07/2000
{}
   SetLCLObject(p, Sender);
   if p <> nil then
   Begin
      gtk_object_set_data(pgtkObject(p),'Style',0);
      gtk_object_set_data(pgtkObject(p),'ExStyle',0);
   end;


{}
//--------------------------

   StrDispose(StrTemp);

   Assert(False, 'Trace:Leaving CreateComponent');
end;

{------------------------------------------------------------------------------}
{       TGtkObject GetLabel                                                    }
{       *Note: Returns a widgets lable value                                   }
{------------------------------------------------------------------------------}
function TgtkObject.GetLabel(CompStyle: Integer; P : Pointer) : String;
var
  pLabel: Pointer; 
  aPChar: PChar;
begin
   Result := 'Label';
   case CompStyle of
     csLabel: begin
                gtk_label_get(PGTKLabel(p),@aPChar);
                Result:=StrPas(aPChar);
              end;

     csForm : Result := StrPas(PgtkWindow(p)^.Title);

     csPage : begin
                 pLabel := gtk_notebook_get_tab_label(
                    PGTKNoteBook(TWinControl(P).Parent.Handle),
                    PGTKWidget(TWinControl(P).Handle));
                 if pLabel <> nil then begin
                   gtk_label_get(pLabel, @aPChar);
                   Result:=StrPas(aPChar);
                 end;
              end;
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
  FormWidget: PGTKWidget;
begin
  FormWidget:=PgtkWidget(TWinControl(Sender).Handle);
//if Sender is TForm then
//writeln('[TgtkObject.ShowHide] START ',Sender.ClassName,' Visible=',TControl(Sender).Visible,' Window=',FormWidget^.Window<>nil);
  if TControl(Sender).Visible then begin
    gtk_widget_show(FormWidget);
    if (Sender is TCustomForm) and (FormWidget^.Window<>nil) then begin
      FormIconGdiObject:=PGDIObject(TCustomForm(Sender).GetIconHandle);
      if (FormIconGdiObject<>nil) then begin
        gdk_window_set_icon(FormWidget^.Window, nil, 
                FormIconGdiObject^.GDIBitmapObject,
                FormIconGdiObject^.GDIBitmapMaskObject);
      end;
    end;
  end
  else Begin
    gtk_widget_hide(FormWidget);
  end;
//if Sender is TForm then
//writeln('[TgtkObject.ShowHide] END ',Sender.ClassName,' Window=',FormWidget^.Window<>nil);
end;

{------------------------------------------------------------------------------}
{       TGtkObject AddNBPage                                                   }
{       *Note: Add Notebook Page                                               }
{------------------------------------------------------------------------------}
procedure TgtkObject.AddNBPage(Parent, Child: TObject; Index: Integer);
var
  pStr: PCHar;
begin
  Assert(false, 'Trace:Adding a notebook page');

  pStr := StrAlloc(Length(TWinControl(Child).Caption) + 1);
  try
    StrPCopy(pStr, TWinControl(Child).Caption);

    gtk_notebook_insert_page(PGtkNotebook(TWinControl(Parent).Handle),
                             PGtkWidget(TWinControl(Child).Handle),
                             gtk_label_new(pStr),
                             Index);
  finally
    strDispose(pStr);
  end;
//   gtk_object_set_data(PGtkObject(TPage(Child).Handle), '1', pgtkwidget(TWinControl(Parent).handle));
end;

{------------------------------------------------------------------------------}
{       TGtkObject RemoveNBPage                                                }
{       *Note: Remove Notebook Page                                            }
{------------------------------------------------------------------------------}
procedure TgtkObject.RemoveNBPage(Parent: TObject; Index: Integer);
begin
  Assert(false, 'Trace:Removing a notebook page');
  gtk_notebook_remove_page(PGtkNotebook(TWinControl(Parent).Handle), Index);
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
   fWindow := pGtkWidget(widget)^.window;
   //gc := gdk_gc_new(PgdkWindow(fWindow));

   gdk_draw_pixmap(fwindow,PGtkStyle(widget^.TheStyle)^.fg_gc[GTK_WIDGET_STATE (widget)], 
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
  PDC     : PDeviceContext;
  Image   : pGDKImage;
  Widget  : PgtkWidget;
  GDKColor: TGDKColor;
  pFixed  : PGTKFixed;
  fWindow : pGdkWindow;
begin
  PDC := PDeviceContext(TCanvas(Sender).Handle);
  if PDC = nil then exit;
  Widget := PgtkWidget(PDC^.HWnd);

  Image := gtk_Object_get_data(pgtkobject(widget),'Image');
  if Image = nil then begin
    Image := gdk_image_get(pgtkWidget(widget)^.window,0,0,
                            widget^.allocation.width,widget^.allocation.height);
    if Image = nil then exit;
    gtk_Object_set_data(pgtkobject(Widget),'Image',Image);
  end;

  GDKColor:=AllocGDKColor(TLMSetGetPixel(data^).PixColor);
//writeln('SetPixel: Color=',HexStr(TLMSetGetPixel(data^).PixColor,8),' GDKColor=',HexStr(GDKColor.Pixel,8));
  gdk_image_put_pixel(Image,TLMSetGetPixel(data^).X,TLMSetGetPixel(data^).Y,
                         GDKColor.Pixel);

  pFixed := GetFixedWidget(Widget);
  if pFixed <> nil then Widget:=PgtkWidget(pFixed);
  fWindow := pGtkWidget(Widget)^.window;
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
  PDC    : PDeviceContext;
  Image  : pGDKImage;
  Widget : PgtkWidget;
  GDKColor: TGDKColor;
  GdkColorContext: PGdkColorContext;
begin
  PDC := PDeviceContext(TCanvas(Sender).Handle);
  if PDC = nil then exit;
  Widget := PgtkWidget(PDC^.HWnd);

  Image := gtk_Object_get_data(pgtkobject(Widget),'Image');
  if Image = nil then begin
    Image := gdk_image_get(pgtkWidget(widget)^.window,0,0,
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
   result := 0;     // default if nobody sets it

   if Sender is TWinControl 
     then Assert(False, Format('Trace:  [TgtkObject.GetValue] %s', [Sender.ClassName]))
     else Assert(False, Format('Trace:WARNING: [TgtkObject.GetValue] %s --> No Decendant of TWinControl', [Sender.ClassName]));

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
      csCheckbox   : if gtk_toggle_button_get_active (PGtkToggleButton (handle))
                        then TCheckBoxState (data^) := cbChecked
                        else TCheckBoxState (data^) := cbUnChecked;
      csCalendar :Begin
                    gtk_calendar_get_date(PgtkCalendar(handle),@Year, @Month, @Day);
                    //TODO:  account for local settings like date format
                    //Form some reason, the month is zero based.
                    TLMCalendar(data^).Date := StrtoDate(Inttostr(Day)+'-'+Inttostr(Month+1)+'-'+Inttostr(Year));
                    

                  end;
   else
      Assert (true, Format ('WARNING:[TgtkObject.GetValue] failed for %s', [Sender.ClassName]));
   end;
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.SetValue
  Params:  Sender : the lcl object which called this func via SenMessage
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
   NUm : Integer;
   
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
   
      csTrackbar   : begin
                        if Handle = nil then Exit;
                        gtk_range_get_adjustment (GTK_RANGE (handle))^.value := integer (data^);
                        gtk_signal_emit_by_name (PGtkObject
                           (gtk_range_get_adjustment (
                               GTK_RANGE (handle))), 'value_changed');
                     end;

      csRadiobutton,
      csCheckbox   : if TCheckBoxState (data^) = cbChecked
                        then gtk_toggle_button_set_active( PGtkToggleButton (handle), TRUE)
                        else gtk_toggle_button_set_active( PGtkToggleButton (handle), FALSE);
      csCalendar   : Begin
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
     csArrow : Begin
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

               end
                     
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
function TgtkObject.SetProperties (Sender : TObject) : integer;
var
   Handle    : Pointer;
   Widget    : PGtkWidget;
   xAlign    : gfloat;
   yAlign    : gfloat;
   I,X         : Integer;
   ColName   : String;
   pColName : PChar;
   pRowText : PChar;
begin
   result := 0;     // default if nobody sets it

   if Sender is TWinControl 
     then Assert(False, Format('Trace:  [TgtkObject.SetProperties] %s', [Sender.ClassName]))
     else Assert(False, Format('Trace:WARNING: [TgtkObject.SetProperties] %s --> No Decendant of TWinControl', [Sender.ClassName]));

   Handle    := Pointer(TWinControl(Sender).Handle);
   Assert (Handle = nil, 'WARNING: [TgtkObject.SetProperties] --> got nil pointer');
   
   case TControl(Sender).fCompStyle of
      csEdit :
          with (TCustomEdit(Sender)) do
             Begin
               gtk_entry_set_editable(PgtkEntry(handle),not(TCustomEdit(sender).ReadOnly));
               gtk_entry_set_max_length(PgtkEntry(handle),TCustomEdit(sender).MaxLength);
             end;

      csProgressBar :
         with (TProgressBar (Sender)) do
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
           case Alignment of
             taLeftJustify  : xAlign := 0.0;
             taCenter       : xAlign := 0.5;
             taRightJustify : xAlign := 1.0;
           else 
             xAlign := 0.0; // default, shouldn't happen
           end;

           case Layout of
             tlTop    : yAlign := 0.0;
             tlCenter : yAlign := 0.5;
             tlBottom : yAlign := 1.0;
           else 
             yAlign := 1.0; //default, shouldn't happen
           end;

           gtk_misc_set_alignment(PGTKMISC(Handle), xAlign, yAlign);
           gtk_label_set_line_wrap(PGTKLABEL(Handle),WordWrap);
         end;
      csListView :
         With TCustomListView(sender) do
            Begin
               //set up columns..
               //
               gtk_clist_freeze(PgtkCList(handle));
               for I := 0 to Columns.Count-1 do
                 Begin
                   ColName := Columns.Strings[i];
                   GetMem(pColName,Length(colname)+1);
                   pColName := StrPcopy(pColName,ColName);
                   gtk_clist_set_column_title(Pgtkclist(handle),I,pColName);
                   Dispose(pColName);
                 end;

                 if (TCustomListView(sender).ViewStyle = vsReport) then
                 gtk_clist_column_titles_show(PgtkCList(handle))
                 else
                 gtk_clist_column_titles_Hide(PgtkCList(handle));

                 gtk_clist_set_sort_column(PgtkCList(handle),TCustomListView(sender).SortColumn);
                 gtk_clist_set_auto_sort(PgtkCList(handle),TCustomListView(sender).Sorted);
               //
               //do items...
               //
               for I := 0 to TCustomListView(sender).Items.Count-1 do
                 Begin

                   GetMem(pRowText,Length(TListItem(TCustomListView(sender).Items[i]).Caption)+1);
                   pRowText := StrPcopy(pRowText,TListItem(TCustomListView(sender).Items[i]).Caption);
                   gtk_clist_set_text(Pgtkclist(handle),0,I+1,pRowText);
                   freemem(pRowText);

                   if (TCustomListView(sender).ViewStyle = vsReport) then //columns showing
                   For X := 1 to Columns.Count-1 do
                      begin
                        if ( X <= TListItem(TCustomListView(sender).Items[i]).SubItems.Count) then
                        Begin
                          GetMem(pRowText,Length(TListItem(TCustomListView(sender).Items[i]).SubItems.Strings[X-1])+1);
                          pRowText := StrPcopy(pRowText,TListItem(TCustomListView(sender).Items[i]).SubItems.Strings[X-1]);
                          gtk_clist_set_text(Pgtkclist(handle),X,I+1,pRowText);
                          freemem(pRowText);
                        end;
                      end;  //for loop



                 end;
                 
               gtk_clist_thaw(PgtkCList(handle));
            end;
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
  MenuParent, MenuItem: Pointer;
begin
  with (Sender as TMenuItem) do
  begin
    MenuItem := Pointer(Handle);
    
    if (Parent.GetParentMenu <> nil) and
       (Parent.GetParentMenu.Items.IndexOf(TMenuItem(Sender)) <> -1) then //mainmenu
    begin
      MenuParent := Pointer(Parent.Handle);
      gtk_menu_bar_append(MenuParent, MenuItem);
    end
    else begin
      // find the menu container
      MenuParent := gtk_object_get_data(PGtkObject(Parent.Handle), 'ContainerMenu');
      if MenuParent = nil then
      begin
        MenuParent := gtk_menu_new;
        gtk_object_set_data(PGtkObject(Parent.Handle), 'ContainerMenu', MenuParent);
        gtk_menu_item_set_submenu(PGTKMenuItem(Parent.Handle), MenuParent);
        
        AccelGroup := gtk_accel_group_new;
        gtk_menu_set_accel_group(MenuParent, AccelGroup);
        SetAccelGroup(MenuParent, AccelGroup);
      end;
      gtk_menu_append(MenuParent, MenuItem);
    end;

    // Add accelerators
    AccelGroup := GetAccelGroup(MenuParent);
    AccelKey := GetAccelKey(MenuItem);
    if (AccelGroup <> nil) and (AccelKey <> 0)
       then gtk_accel_group_add(AccelGroup, AccelKey, GDK_MOD1_MASK, GTK_ACCEL_LOCKED, MenuItem, 'activate_item'); 
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
//        gdiRegion :
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
function TgtkObject.IsValidGDIObjectType(const GDIObject: HGDIOBJ; const GDIType: TGDIType): Boolean;
begin
  Result := IsValidGDIObject(GDIObject) and (PGdiObject(GDIObject)^.GDIType = GDIType);
end;

{------------------------------------------------------------------------------
  Function: NewDC
  Params:  none
  Returns: a gtkwinapi DeviceContext

  Creates an initial DC
 ------------------------------------------------------------------------------}
function TgtkObject.NewDC: PDeviceContext;
begin
  Assert(False, Format('Trace:> [TgtkObject.NewDC]', []));
  New(Result);
  with Result^ do
  begin
    hWnd := 0;
    GC := nil;
    Drawable := nil;
    PenPos.X := 0;
    PenPos.Y := 0;
    CurrentBitmap := nil;
    CurrentFont := nil;
    CurrentPen := nil;
    CurrentBrush := nil;
    SavedContext := nil;
    gdk_color_black(gdk_colormap_get_system, @CurrentTextColor);
    gdk_color_white(gdk_colormap_get_system, @CurrentBackColor);
  end;
  FDeviceContexts.Add(Result);
//writeln('[TgtkObject.NewDC] ',HexStr(Cardinal(Result),8),'  ',FDeviceContexts.Count);
//  Assert(False, Format('Trace:< [TgtkObject.NewDC] FDeviceContexts[%d] --> 0x%p', [n, Result]));
end;
(*
{------------------------------------------------------------------------------
  Function: FreeDC
  Params:  ADC: A DC to Free
  Returns: nothing

  Frees an initial DC
  It does not free the GDI objects. See ReleaseDC for a smarter function.
 ------------------------------------------------------------------------------}
function TgtkObject.FreeDC(ADC: PDeviceContext);
var
  n: Integer;
begin
//writeln('[TgtkObject.FreeDC] ',HexStr(Cardinal(ADC),8));
  Assert(False, Format('Trace:> [TgtkObject.FreeDC] DC:0x%p', [ADC]));
  if ADC <> nil
  then begin
    if ADC^.SavedContext <> nil 
    then begin
      writeln('[TgtkObject.FreeDC] WARNING: there is an unused saved context left!');
      FreeDC(ADC^.SavedContext);
    end;

    Assert(ADC^.CurrentBitmap = nil, 'trace:  [TgtkObject.FreeDC] CurrentBitmap <> nil');
    Assert(ADC^.CurrentFont = nil, 'trace:  [TgtkObject.FreeDC] CurrentFont <> nil'); 
    Assert(ADC^.CurrentPen = nil, 'trace:  [TgtkObject.FreeDC] CurrentPen <> nil');
    Assert(ADC^.CurrentBrush = nil, 'trace:  [TgtkObject.FreeDC] CurrentBrush <> nil');

    if ADC^.GC <> nil 
    then gdk_gc_unref(ADC^.GC);
    n := FDeviceContexts.Remove(ADC);
    Dispose(ADC);
  end;
  
  Assert(False, Format('Trace:< [TgtkObject.FreeDC] FDeviceContexts[%d]', [n]));
end;
*)
{------------------------------------------------------------------------------
  Function: NewGDIObject
  Params:  none
  Returns: a gtkwinapi DeviceContext

  Creates an initial DC
 ------------------------------------------------------------------------------}
function TgtkObject.NewGDIObject(const GDIType: TGDIType): PGdiObject;
begin
  Assert(False, Format('Trace:> [TgtkObject.NewGDIObject]', []));
  New(Result);
  FillChar(Result^, SizeOf(TGDIObject), 0);
  Result^.GDIType := GDIType;
  FGDIObjects.Add(Result);
//writeln('[TgtkObject.NewGDIObject] ',HexStr(Cardinal(Result),8),'  ',FGDIObjects.Count);
  Assert(False, Format('Trace:< [TgtkObject.NewGDIObject] FGDIObjects --> 0x%p', [Result]));
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
  gdk_color_white(gdk_colormap_get_system, @Result^.GDIBrushColor);
end;

{------------------------------------------------------------------------------
  Function: CreateDefaultFont
  Params:  none
  Returns: a Font GDIObject

  Creates an default font, used for initial values
 ------------------------------------------------------------------------------}
function TgtkObject.CreateDefaultFont: PGdiObject;
begin
//write('  TgtkObject.CreateDefaultFont ->');
  Result := NewGDIObject(gdiFont);
  Result^.GDIFontObject := gdk_font_load('-*-helvetica-bold-r-normal--*-120-*-*-*-*-iso8859-1');
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
  gdk_color_black(gdk_colormap_get_system, @Result^.GDIPenColor);
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
  
  procedure ClearTargetLists(Widget: PGtkWidget);
  // MG: Reading in gtk internas is dirty, but there seems to be no other way
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
    ClearTargetLists(ClipboardWidget);
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
        gtk_selection_add_targets(ClipboardWidget,ClipboardTypeAtoms[c],
                  ClipboardTargetEntries[c],ClipboardTargetEntryCnt[c]);
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
