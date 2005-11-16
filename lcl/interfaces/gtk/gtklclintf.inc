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
 *  See the file COPYING.LCL, included in this distribution,                 *
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
  lEventHandler^.OnEvent(lEventHandler^.Handle, condition);
  result := true;
end;

procedure TGtkWidgetSet.AddEventHandler(AHandle: THandle; AFlags: dword; 
  AEventHandler: TWaitHandleEvent);
var
  giochannel: pgiochannel;
  lEventHandler: PWaitHandleEventHandler;
begin
  if AEventHandler = nil then exit;
  New(lEventHandler);
  giochannel := g_io_channel_unix_new(AHandle);
  lEventHandler^.Handle := AHandle;
  lEventHandler^.GIOChannel := giochannel;
  lEventHandler^.OnEvent := AEventHandler;
  lEventHandler^.GSourceID := g_io_add_watch(giochannel,
    AFlags, @waithandle_iocallback, lEventHandler);
  lEventHandler^.NextHandler := FWaitHandles;
  FWaitHandles := lEventHandler;
end;

procedure TGtkWidgetSet.RemoveEventHandler(AHandle: THandle);
var
  lEventHandler: PWaitHandleEventHandler;
  lPrevEventHandler: PPWaitHandleEventHandler;
begin
  lPrevEventHandler := @FWaitHandles;
  while lPrevEventHandler^ <> nil do
  begin
    lEventHandler := lPrevEventHandler^;
    if lEventHandler^.Handle = AHandle then
    begin
      g_source_remove(lEventHandler^.GSourceID);
      lPrevEventHandler^ := lEventHandler^.NextHandler;
      Dispose(lEventHandler);
      exit;
    end;
    lPrevEventHandler := @lEventHandler^.NextHandler;
  end;
end;

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

{------------------------------------------------------------------------------
  function TGTKWidgetSet.FontCanUTF8(Font: HFont): boolean;
  
  True if font recognizes Unicode.
 ------------------------------------------------------------------------------}
function TGTKWidgetSet.FontCanUTF8(Font: HFont): boolean;
begin
  Result:=IsValidGDIObject(Font) and
{$IFDEF GTK2}
 FontIsDoubleByteCharsFont(gdk_font_from_description(PGdiObject(Font)^.GDIFontObject));
{$ELSE GTK2}
 FontIsDoubleByteCharsFont(PGdiObject(Font)^.GDIFontObject);
{$ENDIF GTK2}
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

    // TScrollBar
    if SizeConstraints.Control is TScrollBar then begin
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
    end;
    
    // TCustomSplitter
    if SizeConstraints.Control is TCustomSplitter then begin
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

