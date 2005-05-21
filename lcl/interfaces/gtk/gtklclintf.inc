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
      //DebugLn('TGtkWidgetSet.GetControlConstraints A ',MinWidth,',',MinHeight,' ',TScrollBar(SizeConstraints.Control).Kind=sbHorizontal,' ',TScrollBar(SizeConstraints.Control).Name);
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
  Function: MenuItemSetCheck
  Params:  BaseMenuItem
  Returns: Nothing
  
  Checks or unchecks the specified menu item.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.MenuItemSetCheck(BaseMenuItem: TComponent): Boolean;
var
  IsRadio: Boolean;
  Group: PGSList;
  Item: Pointer;
  AMenuItem: TMenuItem;
begin
  AMenuItem:=BaseMenuItem as TMenuItem;
  Item := Pointer(AMenuItem.Handle);
  IsRadio := gtk_is_radio_menu_item(Item);
  if IsRadio or gtk_is_check_menu_item(Item)
  then begin
    if IsRadio
    then begin
      Group := gtk_radio_menu_item_group(Item);
      LockRadioGroupOnChange(Group, +1);
    end
    else LockOnChange(Item, +1);
    gtk_check_menu_item_set_active(Item, AMenuItem.Checked);
    if IsRadio
    then LockRadioGroupOnChange(Group, -1)
    else LockOnChange(Item, -1);
    Result := True;
  end 
  else begin
    AMenuItem.RecreateHandle;
    Result := True;
  end;
end;

{------------------------------------------------------------------------------
  Function: MenuItemSetEnable
  Params: BaseMenuItem:
  Returns:

  Enables, disables, or grays the specified menu item.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.MenuItemSetEnable(BaseMenuItem: TComponent): Boolean;
var
  AMenuItem: TMenuItem;
begin
  AMenuItem:=BaseMenuItem as TMenuItem;
  gtk_widget_set_sensitive(pgtkwidget(AMenuItem.Handle),
                           AMenuItem.Enabled and (AMenuItem.Caption<>'-'));
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: RightJustifyMenuItem
  Params:  HndMenu: HMenu; bRightJustify: boolean
  Returns: true on success

  Sets left or right justification of a menuitem
-------------------------------------------------------------------------------}
function TGtkWidgetSet.RightJustifyMenuItem(BaseMenuItem: TComponent): Boolean;
var
  AMenuItem: TMenuItem;
  MenuItemWidget: PGtkMenuItem;
begin
  AMenuItem:=BaseMenuItem as TMenuItem;
  MenuItemWidget:=PGtkMenuItem(AMenuItem.Handle);
  gtk_menu_item_set_right_justified(MenuItemWidget, AMenuItem.RightJustify);
  gtk_widget_queue_resize(GTK_WIDGET(MenuItemWidget));
  Result:=false;
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

{ =============================================================================

  $Log$
  Revision 1.38  2005/05/21 15:58:44  mattias
  implemented right justification for menuitems for winapi intf  from Martin Smat

  Revision 1.37  2005/03/31 21:29:45  micha
  move GetNotebookTabIndexAtPos to the widgetset interface
  implement it for win32 widgetset (fixes bug 669)

  Revision 1.36  2005/02/23 01:12:46  marc
  + Added RemoveProp winapi call
  * Some maintenace on winapi/lclintf files

  Revision 1.35  2005/01/12 23:18:07  mattias
  limited widget sizes to 10000x10000

  Revision 1.34  2004/10/23 14:47:44  micha
  remove old code: statusbar methods in twidgetset

  Revision 1.33  2004/09/12 18:55:13  mazen
  * Fix font  retreiving from Pango description

  Revision 1.32  2004/09/10 16:28:51  mattias
  implemented very rudimentary TTabControl

  Revision 1.31  2004/09/04 22:24:16  mattias
  added default values for compiler skip options and improved many parts of synedit for UTF8

  Revision 1.30  2004/09/02 09:17:00  mattias
  improved double byte char fonts for gtk1, started synedit UTF8 support

  Revision 1.29  2004/06/19 21:06:38  mattias
  menu separators are now created disabled

  Revision 1.28  2004/05/22 14:59:23  mattias
  fixed multi update of TStatusBar

  Revision 1.27  2004/05/11 12:16:47  mattias
  replaced writeln by debugln

  Revision 1.26  2004/04/02 20:20:21  mattias
  implemented by guessing the 16bit bmp reader part

  Revision 1.25  2004/03/30 20:38:14  mattias
  fixed interface constraints, fixed syncompletion colors

  Revision 1.24  2004/03/28 12:49:22  mattias
  implemented mask merge and extraction for raw images

  Revision 1.23  2004/03/24 01:21:41  marc
  * Simplified signals for gtkwsbutton

  Revision 1.22  2004/03/22 19:10:04  mattias
  implemented icons for TPage in gtk, mask for TCustomImageList

  Revision 1.21  2004/03/19 00:53:34  marc
  * Removed all ComponentCreateHandle routines

  Revision 1.20  2004/03/19 00:03:15  marc
  * Moved the implementation of (GTK)ButtonCreateHandle to the new
    (GTK)WSButton class

  Revision 1.19  2004/03/09 15:30:15  peter
    * fixed gtk2 compilation

  Revision 1.18  2004/03/05 00:31:52  marc
  * Renamed TGtkObject to TGtkWidgetSet

  Revision 1.17  2004/02/28 00:34:35  mattias
  fixed CreateComponent for buttons, implemented basic Drag And Drop

  Revision 1.16  2004/02/27 00:42:41  marc
  * Interface CreateComponent splitup
  * Implemented CreateButtonHandle on GTK interface
    on win32 interface it still needs to be done
  * Changed ApiWizz to support multilines and more interfaces

  Revision 1.15  2004/02/17 00:32:25  mattias
  fixed TCustomImage.DoAutoSize fixing uninitialized vars

  Revision 1.14  2004/02/02 15:46:19  mattias
  implemented basic TSplitter, still many ToDos

  Revision 1.13  2004/02/02 12:44:45  mattias
  implemented interface constraints

  Revision 1.12  2004/01/22 11:23:36  mattias
  started MaskBlt for gtkIF and applied patch for dir dlg in env opts from Vincent

  Revision 1.11  2004/01/12 13:43:12  mattias
  improved and activated new statusbar

  Revision 1.10  2004/01/12 08:36:34  micha
  statusbar interface dependent reimplementation (from vincent)

  Revision 1.9  2004/01/11 16:38:29  marc
  * renamed (Check|Enable)MenuItem to MenuItemSet(Check|Enable)
  + Started with accelerator nameing routines
  * precheckin for createwidget splitup

  Revision 1.8  2004/01/11 11:57:54  mattias
  implemented TCustomListBox.ItemRect for gtk1 intf

  Revision 1.7  2004/01/09 20:03:13  mattias
  implemented new statusbar methods in gtk intf

  Revision 1.6  2004/01/04 16:44:33  mattias
  updated gtk2 package

  Revision 1.5  2004/01/03 11:57:48  mattias
  applied implementation for LM_LB_GETINDEXAT  from Vincent

  Revision 1.4  2003/11/27 23:02:30  mattias
  removed menutype.pas

  Revision 1.3  2003/11/26 21:30:19  mattias
  reduced unit circles, fixed fpImage streaming

  Revision 1.2  2003/11/26 00:23:47  marc
  * implemented new LCL(check|enable)Menuitem functions
  * introduced the lclintf inc files to win32

  Revision 1.1  2003/11/24 11:03:07  marc
  * Splitted winapi*.inc into a winapi and a lcl interface communication part

}
