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

  if TCustomListbox(ListBox).FCompStyle in [csListBox, csCheckListBox] then begin
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
  Function: LCLCheckMenuItem
  Params:  BaseMenuItem
  Returns: Nothing
  
  Checks or unchecks the specified menu item.
 ------------------------------------------------------------------------------}
function TgtkObject.LclCheckMenuItem(BaseMenuItem: TComponent): Boolean;
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
  Function: LCLEnableMenuItem
  Params: BaseMenuItem:
  Returns:

  Enables, disables, or grays the specified menu item.
 ------------------------------------------------------------------------------}
function TGTKObject.LCLEnableMenuItem(BaseMenuItem: TComponent): Boolean;
var
  AMenuItem: TMenuItem;
begin
  AMenuItem:=BaseMenuItem as TMenuItem;
  gtk_widget_set_sensitive(pgtkwidget(AMenuItem.Handle), AMenuItem.Enabled);
  Result := True;
end;

{------------------------------------------------------------------------------
  Procedure: StatusBarInsertPanel
  Params: StatusBar:
          index:
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TGTKObject.StatusBarInsertPanel(StatusBar: TObject; Index: integer);
begin
  UpdateStatusBarPanels(StatusBar,PGtkWidget((StatusBar as TStatusBar).Handle));
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
  Procedure: StatusBarRemovePanel
  Params: StatusBar:
          index:
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TGTKObject.StatusBarRemovePanel(StatusBar: TObject; Index: integer);
begin
  UpdateStatusBarPanels(StatusBar,PGtkWidget((StatusBar as TStatusBar).Handle));
end;

{------------------------------------------------------------------------------
  Procedure: StatusBarSetSimpleText
  Params: StatusBar:
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TGTKObject.StatusBarSetSimpleText(StatusBar: TObject);
begin
  UpdateStatusBarPanels(StatusBar,PGtkWidget((StatusBar as TStatusBar).Handle));
end;

{------------------------------------------------------------------------------
  Procedure: StatusBarSetStyle
  Params: StatusBar:
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TGTKObject.StatusBarSetStyle(StatusBar: TObject);
begin
  UpdateStatusBarPanels(StatusBar,PGtkWidget((StatusBar as TStatusBar).Handle));
end;


//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

{ =============================================================================

  $Log$
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
