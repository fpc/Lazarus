{%MainUnit gtkint.pp}
{ $Id$ }
{******************************************************************************
                 All GTK interface communication implementations.
                   Initial Revision  : Sun Nov 23 23:53:53 2003


  !! Keep alphabetical !!

  Support routines go to gtkproc.pp

 ******************************************************************************
 Implementation
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

//##apiwiz##sps##   // Do not remove

function waithandle_iocallback(source: PGIOChannel; condition: TGIOCondition; 
  data: gpointer): gboolean; cdecl;
var
  lEventHandler: PWaitHandleEventHandler absolute data;
begin
  //debugln('waithandle_iocallback lEventHandler=',HexStr(Cardinal(lEventHandler),8));
  lEventHandler^.OnEvent(lEventHandler^.UserData, condition);
  Result := true;
end;

function TGtkWidgetSet.AddEventHandler(AHandle: THandle; AFlags: dword; 
  AEventHandler: TWaitHandleEvent; AData: PtrInt): PEventHandler;
var
  giochannel: pgiochannel;
  lEventHandler: PWaitHandleEventHandler;
begin
  if AEventHandler = nil then exit;
  New(lEventHandler);
  giochannel := g_io_channel_unix_new(AHandle);
  lEventHandler^.Handle := AHandle;
  lEventHandler^.UserData := AData;
  lEventHandler^.GIOChannel := giochannel;
  lEventHandler^.OnEvent := AEventHandler;
  lEventHandler^.GSourceID := g_io_add_watch(giochannel,
    AFlags, @waithandle_iocallback, lEventHandler);
  //debugln('TGtkWidgetSet.AddEventHandler lEventHandler=',HexStr(Cardinal(lEventHandler),8),' AHandle=',dbgs(lEventHandler^.Handle));
  lEventHandler^.PrevHandler := nil;
  lEventHandler^.NextHandler := FWaitHandles;
  if FWaitHandles <> nil then
    FWaitHandles^.PrevHandler := lEventHandler;
  FWaitHandles := lEventHandler;
  Result := lEventHandler;
end;

procedure TGtkWidgetSet.RemoveEventHandler(var AHandler: PEventHandler);
var
  lEventHandler: PWaitHandleEventHandler absolute AHandler;
begin
  if AHandler = nil then exit;
  g_source_remove(lEventHandler^.GSourceID);
  { channel will be freed with ref count drops to 0 }
  g_io_channel_unref(lEventHandler^.GIOChannel);
  if lEventHandler^.PrevHandler = nil then
    FWaitHandles := lEventHandler^.NextHandler
  else
    lEventHandler^.PrevHandler^.NextHandler := lEventHandler^.NextHandler;
  if lEventHandler^.NextHandler <> nil then
    lEventHandler^.NextHandler^.PrevHandler := lEventHandler^.PrevHandler;
  //debugln('TGtkWidgetSet.RemoveEventHandler lEventHandler=',HexStr(Cardinal(lEventHandler),8),' AHandle=',dbgs(lEventHandler^.Handle));
  Dispose(lEventHandler);
  AHandler := nil;
end;

procedure TGtkWidgetSet.SetEventHandlerFlags(AHandler: PEventHandler; NewFlags: dword);
var
  lEventHandler: PWaitHandleEventHandler absolute AHandler;
begin
  if AHandler = nil then exit;
  g_source_remove(lEventHandler^.GSourceID);
  lEventHandler^.GSourceID := g_io_add_watch(lEventHandler^.GIOChannel,
    NewFlags, @waithandle_iocallback, lEventHandler);
  //debugln('TGtkWidgetSet.SetEventHandlerFlags lEventHandler=',HexStr(Cardinal(lEventHandler),8),' AHandle=',dbgs(lEventHandler^.Handle));
end;

type
  PPipeEventInfo = ^TPipeEventInfo;
  TPipeEventInfo = record
    Handler: PEventHandler;
    UserData: PtrInt;
    OnEvent: TPipeEvent;
  end;

function TGtkWidgetSet.AddPipeEventHandler(AHandle: THandle; 
  AEventHandler: TPipeEvent; AData: PtrInt): PPipeEventHandler;
var
  lPipeEventInfo: PPipeEventInfo;
begin
  if AEventHandler = nil then exit;
  New(lPipeEventInfo);
  lPipeEventInfo^.UserData := AData;
  lPipeEventInfo^.OnEvent := AEventHandler;
  lPipeEventInfo^.Handler := AddEventHandler(AHandle, G_IO_IN or G_IO_HUP or G_IO_OUT, 
    @HandlePipeEvent, PtrInt(lPipeEventInfo));
  Result := lPipeEventInfo;
end;

procedure TGtkWidgetSet.HandlePipeEvent(AData: PtrInt; AFlags: dword);
var
  lPipeEventInfo: PPipeEventInfo absolute AData;
  lReasons: TPipeReasons;
begin
  lReasons := [];
  if AFlags and G_IO_IN = G_IO_IN then
    Include(lReasons, prDataAvailable);
  if AFlags and G_IO_OUT = G_IO_OUT then
    Include(lReasons, prCanWrite);
  if AFlags and G_IO_HUP = G_IO_HUP then
    Include(lReasons, prBroken);
  
  lPipeEventInfo^.OnEvent(lPipeEventInfo^.UserData, lReasons);
end;

procedure TGtkWidgetSet.RemovePipeEventHandler(var AHandler: PPipeEventHandler);
var
  lPipeEventInfo: PPipeEventInfo absolute AHandler;
begin
  if AHandler = nil then exit;
  RemoveEventHandler(lPipeEventInfo^.Handler);
  Dispose(lPipeEventInfo);
  AHandler := nil;
end;

{$ifdef UNIX}
function TGtkWidgetSet.AddProcessEventHandler(AHandle: THandle;
  AEventHandler: TChildExitEvent; AData: PtrInt): PProcessEventHandler;
var  
  lHandler: PChildSignalEventHandler;
begin
  if AEventHandler = nil then exit;
  New(lHandler);
  lHandler^.PID := TPid(AHandle);
  lHandler^.UserData := AData;
  lHandler^.OnEvent := AEventHandler;
  lHandler^.PrevHandler := nil;
  lHandler^.NextHandler := FChildSignalHandlers;
  if FChildSignalHandlers <> nil then
    FChildSignalHandlers^.PrevHandler := lHandler;
  FChildSignalHandlers := lHandler;
  Result := lHandler;
end;

procedure TGtkWidgetSet.RemoveProcessEventHandler(var AHandler: PProcessEventHandler);
var
  lHandler: PChildSignalEventHandler absolute AHandler;
begin
  if AHandler = nil then exit;
  if lHandler^.PrevHandler = nil then
    FChildSignalHandlers := lHandler^.NextHandler
  else
    lHandler^.PrevHandler^.NextHandler := lHandler^.NextHandler;
  if lHandler^.NextHandler <> nil then
    lHandler^.NextHandler^.PrevHandler := lHandler^.PrevHandler;
  Dispose(lHandler);
  AHandler := nil;
end;
{$else}
{$warning TGtkWidgetSet.RemoveProcessEventHandler and TGtkWidgetSet.AddProcessEventHandler not implemented on this OS}
//PChildSignalEventHandler is only defined on unix
function TGtkWidgetSet.AddProcessEventHandler(AHandle: THandle;
  AEventHandler: TChildExitEvent; AData: PtrInt): PProcessEventHandler;
begin
end;

procedure TGtkWidgetSet.RemoveProcessEventHandler(var AHandler: PProcessEventHandler);
begin
end;
{$endif}

{------------------------------------------------------------------------------
  function TGtkWidgetSet.DrawSplitter(DC: HDC; const ARect: TRect;
    Horizontal: boolean): Integer;

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.DrawSplitter(DC: HDC; const ARect: TRect;
  Horizontal: boolean): boolean;
var
  Widget: PGtkWidget;
  ClientWidget: Pointer;
  DCOrigin: TPoint;
  Detail: PChar;
  Area: TGdkRectangle;
  Style: PGtkStyle;
  AWindow: PGdkWindow;
begin
  Result := False;
  if not IsValidDC(DC) then exit;

  Widget:=PGtkWidget(TDeviceContext(DC).Wnd);
  ClientWidget:=GetFixedWidget(Widget);
  if ClientWidget<>nil then
    Widget:=ClientWidget;
  AWindow:=TDeviceContext(DC).Drawable;

  Style:=GetStyle(lgsButton);
  if Horizontal then begin
    Detail:='hpaned';
  end else begin
    Detail:='vpaned';
  end;

  DCOrigin:=GetDCOffset(TDeviceContext(DC));
  Area.X:=ARect.Left+DCOrigin.X;
  Area.Y:=ARect.Top+DCOrigin.Y;
  Area.Width:=ARect.Right-ARect.Left;
  Area.Height:=ARect.Bottom-ARect.Top;

  gtk_paint_box(Style, AWindow,
    GTK_WIDGET_STATE(Widget),
    GTK_SHADOW_OUT,
    @Area, Widget, Detail,
    Area.X,Area.Y,Area.Width,Area.Height);

  Result:=true;
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint;
    Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;

  As ExtTextOut except that Str is treated as UTF8
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  IsDBCSFont: Boolean;
  NewCount: Integer;
begin
  UpdateDCTextMetric(TDeviceContext(DC));
  IsDBCSFont:=TDeviceContext(DC).DCTextMetric.IsDoubleByteChar;
  if IsDBCSFont then begin
    NewCount:=Count*2;
    if FExtUTF8OutCacheSize<NewCount then begin
      ReAllocMem(FExtUTF8OutCache,NewCount);
      FExtUTF8OutCacheSize:=NewCount;
    end;
    NewCount:=UTF8ToDoubleByte(Str,Count,FExtUTF8OutCache)*2;
    //debugln('TGtkWidgetSet.ExtUTF8Out Count=',dbgs(Count),' NewCount=',dbgs(NewCount));
    Result:=ExtTextOut(DC,X,Y,Options,Rect,FExtUTF8OutCache,NewCount,Dx);
  end else begin
    Result:=ExtTextOut(DC,X,Y,Options,Rect,Str,Count,Dx);
  end;
end;

function TGtkWidgetSet.TextUTF8Out(DC: HDC; X, Y: Integer; Str: PChar; Count: Longint): Boolean;
var
  IsDBCSFont: Boolean;
  NewCount: Integer;
begin
  UpdateDCTextMetric(TDeviceContext(DC));
  IsDBCSFont:=TDeviceContext(DC).DCTextMetric.IsDoubleByteChar;
  if IsDBCSFont then begin
    NewCount:=Count*2;
    if FExtUTF8OutCacheSize<NewCount then begin
      ReAllocMem(FExtUTF8OutCache,NewCount);
      FExtUTF8OutCacheSize:=NewCount;
    end;
    NewCount:=UTF8ToDoubleByte(Str,Count,FExtUTF8OutCache)*2;
    Result:=TextOut(DC,X,Y,FExtUTF8OutCache,NewCount);
  end else begin
    Result:=TextOut(DC,X,Y,Str,Count);
  end;
end;

{------------------------------------------------------------------------------
  function TGTKWidgetSet.FontCanUTF8(Font: HFont): boolean;
  
  True if font recognizes Unicode.
 ------------------------------------------------------------------------------}
function TGTKWidgetSet.FontCanUTF8(Font: HFont): boolean;
begin
  Result:=IsValidGDIObject(Font)
    {$IFDEF Gtk1}
    and FontIsDoubleByteCharsFont(PGdiObject(Font)^.GDIFontObject)
    {$ENDIF}
    ;
end;

{------------------------------------------------------------------------------
  Function: GetAcceleratorString
  Params: AVKey:
          AShiftState:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetAcceleratorString(const AVKey: Byte;
  const AShiftState: TShiftState): String;
begin
  Result:='';
end;

{------------------------------------------------------------------------------
  Function: GetControlConstraints
  Params: Constraints: TObject
  Returns: true on success

  Updates the constraints object (e.g. TSizeConstraints) with interface specific
  bounds.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetControlConstraints(Constraints: TObject): boolean;
var
  SizeConstraints: TSizeConstraints;
  Widget: PGtkWidget;
  MinWidth: Integer;
  MinHeight: Integer;
  MaxWidth: Integer;
  MaxHeight: Integer;
begin
  Result:=true;
  if Constraints is TSizeConstraints then begin
    MinWidth := 0;
    MinHeight := 0;
    MaxWidth:=10000;
    MaxHeight:=10000;
    SizeConstraints:=TSizeConstraints(Constraints);

    if (SizeConstraints.Control=nil) then exit;

    if SizeConstraints.Control is TScrollBar then begin
      // TScrollBar
      if TScrollBar(SizeConstraints.Control).Kind=sbHorizontal then begin
        Widget:=GetStyleWidget(lgsHorizontalScrollbar);
        MinHeight:=Widget^.requisition.Height;
      end else begin
        Widget:=GetStyleWidget(lgsVerticalScrollbar);
        MinWidth:=Widget^.requisition.Width;
      end;
      //DebugLn('TGtkWidgetSet.GetControlConstraints A '+dbgs(MinWidth)+','+dbgs(MinHeight),' ',dbgs(TScrollBar(SizeConstraints.Control).Kind=sbHorizontal),' ',TScrollBar(SizeConstraints.Control).Name);
      SizeConstraints.SetInterfaceConstraints(MinWidth,MinHeight,
                                              MinWidth,MinHeight);
      exit;
    end
    else if SizeConstraints.Control is TCustomSplitter then begin
      // TCustomSplitter
      if TCustomSplitter(SizeConstraints.Control).Align in [alTop,alBottom] then
      begin
        Widget:=GetStyleWidget(lgsHorizontalPaned);
        MinHeight:=Widget^.requisition.Height;
      end else begin
        Widget:=GetStyleWidget(lgsVerticalPaned);
        MinWidth:=Widget^.requisition.Width;
      end;
      SizeConstraints.SetInterfaceConstraints(MinWidth,MinHeight,
                                              MinWidth,MinHeight);
      exit;
    end
    else if SizeConstraints.Control is TCustomMemo then begin
      // TCustomMemo
      Widget:=GetStyleWidget(lgsHorizontalScrollbar);
      MinHeight:=Widget^.requisition.Height+20;
      Widget:=GetStyleWidget(lgsVerticalScrollbar);
      MinWidth:=Widget^.requisition.Width+20;
    end
    else if SizeConstraints.Control is TCustomTrackBar then begin
      // TCustomTrackBar
      if TCustomTrackBar(SizeConstraints.Control).Orientation=trHorizontal then
      begin
        Widget:=GetStyleWidget(lgsHScale);
        MinHeight:=Widget^.requisition.height;
      end else begin
        Widget:=GetStyleWidget(lgsVScale);
        MinWidth:=Widget^.requisition.width;
      end;
      //DebugLn(['TGtkWidgetSet.GetControlConstraints ',DbgSName(SizeConstraints.Control),' ',MinWidth,',',MinHeight]);
    end;

    SizeConstraints.SetInterfaceConstraints(MinWidth,MinHeight,
                                            MaxWidth,MaxHeight);
  end;
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.GetLCLOwnerObject(Handle: HWnd): TObject;

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetLCLOwnerObject(Handle: HWnd): TObject;
begin
  if Handle<>0 then
    Result:=GetNearestLCLObject(PGtkWidget(Handle))
  else
    Result:=nil;
end;

{------------------------------------------------------------------------------
  Function: GetListBoxIndexAtY
  Params: ListBox:
          y:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetListBoxIndexAtY(ListBox: TComponent; y: integer): integer;
{$IFdef GTK2}
var
  aTreeView: PGtkTreeView;
  aTreeColumn: PGtkTreeViewColumn;
  aTreePath   : PGtkTreePath;
  AWinControl: TWinControl;
begin
  Result:=-1;
  if not (ListBox is TWinControl) then exit;
  AWinControl:=TWinControl(ListBox);
  case AWinControl.fCompStyle of

  csListBox, csCheckListBox:
    begin
      aTreeView :=
        GTK_TREE_VIEW(GetWidgetInfo(Pointer(AWinControl.Handle), True)
          ^.CoreWidget);

      if gtk_tree_view_get_path_at_pos(aTreeView, 0, Y, aTreePath, aTreeColumn,
        nil, nil)
      then begin
        Result := gtk_tree_path_get_indices(aTreePath)[0];
        gtk_tree_path_free(aTreePath);
        exit;
      end;
    end;
  end;
end;
{$Else}
var
  ScrolledWindow: PGtkScrolledWindow;
  VertAdj: PGTKAdjustment;
  AdjValue: integer;
  ListWidget: PGtkList;
  AWidget: PGtkWidget;
  GListItem: PGList;
  ListItemWidget: PGtkWidget;
begin
  Result:=-1;
  if not (ListBox is TCustomListbox) then exit;

  if TCustomListbox(ListBox).FCompStyle in [csListBox, csCheckListBox] then
  begin
    AWidget:=PGtkWidget(TCustomListbox(ListBox).Handle);
    ListWidget:=PGtkList(GetWidgetInfo(AWidget, True)^.CoreWidget);
    ScrolledWindow:=PGtkScrolledWindow(AWidget);
    VertAdj:=gtk_scrolled_window_get_vadjustment(ScrolledWindow);
    if VertAdj=nil then
      AdjValue:=y
    else
      AdjValue:=RoundToInt(VertAdj^.value)+y;
    GListItem:=ListWidget^.children;
    while GListItem<>nil do begin
      inc(Result);
      ListItemWidget:=PGtkWidget(GListItem^.data);
      dec(AdjValue,ListItemWidget^.Allocation.Height);
      if AdjValue<0 then exit;
      GListItem:=GListItem^.next;
    end;
    Result:=-1;
  end;

end;
{$EndIf}

{------------------------------------------------------------------------------
  function TGtkWidgetSet.GetListBoxItemRect(ListBox: TComponent; Index: integer;
    var ARect: TRect): boolean;

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetListBoxItemRect(ListBox: TComponent; Index: integer;
  var ARect: TRect): boolean;
{$IFdef GTK2}
var
  AWinControl: TWinControl;
begin
  Result:=false;
  FillChar(ARect,SizeOf(ARect),0);
  if not (ListBox is TWinControl) then exit;
  AWinControl:=TWinControl(ListBox);
  case AWinControl.fCompStyle of

  csListBox, csCheckListBox:
    begin
      // ToDo
    end;
  end;
end;
{$Else}
var
  ScrolledWindow: PGtkScrolledWindow;
  VertAdj: PGTKAdjustment;
  AdjValue: integer;
  ListWidget: PGtkList;
  AWidget: PGtkWidget;
  GListItem: PGList;
  ListItemWidget: PGtkWidget;
begin
  Result:=false;
  FillChar(ARect,SizeOf(ARect),0);
  if not (ListBox is TCustomListbox) then exit;

  if TCustomListbox(ListBox).FCompStyle in [csListBox, csCheckListBox] then
  begin
    AWidget:=PGtkWidget(TCustomListbox(ListBox).Handle);
    ListWidget:=PGtkList(GetWidgetInfo(AWidget, True)^.CoreWidget);
    ScrolledWindow:=PGtkScrolledWindow(AWidget);
    VertAdj:=gtk_scrolled_window_get_vadjustment(ScrolledWindow);
    if VertAdj=nil then
      AdjValue:=0
    else
      AdjValue:= (-RoundToInt(VertAdj^.value));
    GListItem:=ListWidget^.children;
    while GListItem<>nil do begin
      ListItemWidget:=PGtkWidget(GListItem^.data);
      if Index=0 then begin
        ARect.Left:=0;
        ARect.Top:=AdjValue;
        ARect.Right:=ListItemWidget^.Allocation.Width;
        ARect.Bottom:=ARect.Top+ListItemWidget^.Allocation.Height;
        Result:=true;
        exit;
      end;
      inc(AdjValue,ListItemWidget^.Allocation.Height);
      dec(Index);
      GListItem:=GListItem^.next;
    end;
  end;
end;
{$EndIf}

{------------------------------------------------------------------------------
  function TGtkWidgetSet.IntfSendsUTF8KeyPress: boolean;

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.IntfSendsUTF8KeyPress: boolean;
begin
  Result:=true;
end;

{------------------------------------------------------------------------------
  Procedure: ReplaceBitmapMask
  Params: Image    The HBitmap of the image
          Mask     The HBitmap of the mask (will be freed)
          NewMask  The HBitmap of the new mask. Will be merged into Image.
                   if NewMask is 0 the mask of Image is deleted.
  Returns: True on success
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ReplaceBitmapMask(var Image, Mask: HBitmap;
  NewMask: HBitmap): boolean;
var
  ImageGDIObject: PGDIObject;
  NewMaskGDIObject: PGDIObject;
  MaskDescription: TRawImageDescription;
begin
  Result:=IsValidGDIObject(Image) and
          ((NewMask=0) or IsValidGDIObject(NewMask));
  DebugLn('TGtkWidgetSet.ReplaceBitmapMask A ',dbgs(Result));
  if not Result then exit;
  
  // free 'Mask'
  if Mask<>0 then begin
    DeleteObject(Mask);
    Mask:=0;
  end;
  
  // free old mask in 'Image'
  ImageGDIObject:=PGdiObject(Image);
  if ImageGDIObject^.GDIBitmapMaskObject<>nil then begin
    gdk_pixmap_unref(ImageGDIObject^.GDIBitmapMaskObject);
    ImageGDIObject^.GDIBitmapMaskObject:=nil;
    DebugLn('TGtkWidgetSet.ReplaceBitmapMask B old Mask deleted');
  end;

  // move image data from 'NewMask' to mask data of 'Image'
  if NewMask=0 then exit;
  NewMaskGDIObject:=PGDIObject(NewMask);
  if NewMaskGDIObject^.GDIBitmapType<>gbBitmap then begin
    RaiseGDBException('TGtkWidgetSet.ReplaceBitmapMask: invalid Mask Depth');
  end;
  if NewMaskGDIObject^.GDIBitmapObject<>nil then begin
    if not GetWindowRawImageDescription(NewMaskGDIObject^.GDIBitmapObject,
                                 @MaskDescription) then exit;
    if MaskDescription.Depth<>1 then begin
      RaiseGDBException('TGtkWidgetSet.ReplaceBitmapMask: invalid Mask Depth');
    end;
    ImageGDIObject^.GDIBitmapMaskObject:=NewMaskGDIObject^.GDIBitmapObject;
    NewMaskGDIObject^.GDIBitmapObject:=nil;
    DebugLn('TGtkWidgetSet.ReplaceBitmapMask C Mask replaced');
  end;

  // delete GDIObject 'NewMask'
  DeleteObject(NewMask);
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
