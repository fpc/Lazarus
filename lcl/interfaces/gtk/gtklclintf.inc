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
  Function: GetAcceleratorString
  Params: AVKey:
          AShiftState:
  Returns:

 ------------------------------------------------------------------------------}
function TGTKObject.GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String;
begin
  // Your code here
end;

{------------------------------------------------------------------------------
  Function: GetListBoxIndexAtY
  Params: ListBox:
          y:
  Returns:

 ------------------------------------------------------------------------------}
function TGTKObject.GetListBoxIndexAtY(ListBox: TComponent; y: integer): integer;
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
          ^.ImplementationWidget);

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
    ListWidget:=PGtkList(GetWidgetInfo(AWidget, True)^.ImplementationWidget);
    ScrolledWindow:=PGtkScrolledWindow(AWidget);
    VertAdj:=gtk_scrolled_window_get_vadjustment(ScrolledWindow);
    if VertAdj=nil then
      AdjValue:=y
    else
      AdjValue:=round(VertAdj^.value)+y;
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
  function TGTKObject.GetListBoxItemRect(ListBox: TComponent; Index: integer;
    var ARect: TRect): boolean;

 ------------------------------------------------------------------------------}
function TGTKObject.GetListBoxItemRect(ListBox: TComponent; Index: integer;
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
    ListWidget:=PGtkList(GetWidgetInfo(AWidget, True)^.ImplementationWidget);
    ScrolledWindow:=PGtkScrolledWindow(AWidget);
    VertAdj:=gtk_scrolled_window_get_vadjustment(ScrolledWindow);
    if VertAdj=nil then
      AdjValue:=0
    else
      AdjValue:= (-round(VertAdj^.value));
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
  Function: MenuItemSetCheck
  Params:  BaseMenuItem
  Returns: Nothing
  
  Checks or unchecks the specified menu item.
 ------------------------------------------------------------------------------}
function TgtkObject.MenuItemSetCheck(BaseMenuItem: TComponent): Boolean;
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
function TGTKObject.MenuItemSetEnable(BaseMenuItem: TComponent): Boolean;
var
  AMenuItem: TMenuItem;
begin
  AMenuItem:=BaseMenuItem as TMenuItem;
  gtk_widget_set_sensitive(pgtkwidget(AMenuItem.Handle), AMenuItem.Enabled);
  Result := True;
end;

{------------------------------------------------------------------------------
  Procedure: StatusBarPanelUpdate
  Params: StatusBar:
          index:
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TGTKObject.StatusBarPanelUpdate(StatusBar: TObject; Index: integer);
var
  AStatusBar: TStatusBar;
  HBox: PGtkWidget;
  StatusPanelWidget: PGtkWidget;
begin
  AStatusBar:=StatusBar as TStatusBar;
  if Index>=0 then begin
    // update one
    HBox:=PGtkWidget(AStatusBar.Handle);
    StatusPanelWidget:=PGtkBoxChild(
                        g_list_nth_data(PGtkBox(HBox)^.children,Index))^.Widget;
    UpdateStatusBarPanel(StatusBar,Index,StatusPanelWidget);
  end else begin
    // update all
    UpdateStatusBarPanels(StatusBar,PGtkWidget(AStatusBar.Handle));
  end;
end;

{------------------------------------------------------------------------------
  Procedure: StatusBarSetText
  Params: StatusBar:
          PanelIndex:
          Text:
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TGTKObject.StatusBarSetText(StatusBar: TObject; PanelIndex: integer);
var
  AStatusBar: TStatusBar;
  HBox: PGtkWidget;
  StatusPanelWidget: PGtkWidget;
begin
  AStatusBar := StatusBar as TStatusBar;
  if AStatusBar.SimplePanel then
    UpdateStatusBarPanels(StatusBar,PGtkWidget(AStatusBar.Handle))
  else begin
    HBox:=PGtkWidget(AStatusBar.Handle);
    StatusPanelWidget:=PGtkBoxChild(
                        g_list_nth_data(PGtkBox(HBox)^.children,PanelIndex))^.Widget;
    UpdateStatusBarPanel(StatusBar,PanelIndex,StatusPanelWidget);
  end;
end;

{------------------------------------------------------------------------------
  Procedure: StatusBarUpdate
  Params: StatusBar:
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TGTKObject.StatusBarUpdate(StatusBar: TObject);
begin
  UpdateStatusBarPanels(StatusBar,PGtkWidget((StatusBar as TStatusBar).Handle));
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

{ =============================================================================

  $Log$
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
