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
  Function: AlignmentCreateHandle
  Params: AAlignment:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.AlignmentCreateHandle(const AAlignment: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: ArrowCreateHandle
  Params: AArrow:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ArrowCreateHandle(const AArrow: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: BitBtnCreateHandle
  Params: ABitBtn:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.BitBtnCreateHandle(const ABitBtn: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: CalendarCreateHandle
  Params: ACalendar:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CalendarCreateHandle(const ACalendar: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: CheckboxCreateHandle
  Params: ACheckBox:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CheckboxCreateHandle(const ACheckBox: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: CheckListBoxCreateHandle
  Params: ACheckListBox:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CheckListBoxCreateHandle(const ACheckListBox: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: CListBoxCreateHandle
  Params: ACListBox:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.CListBoxCreateHandle(const ACListBox: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: ColorDialogCreateHandle
  Params: AColorDialog:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ColorDialogCreateHandle(const AColorDialog: TObject): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: ComboBoxCreateHandle
  Params: AComboBox:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ComboBoxCreateHandle(const AComboBox: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
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
  Function: EditCreateHandle
  Params: AEdit:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.EditCreateHandle(const AEdit: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: FileDialogCreateHandle
  Params: AFileDialog:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.FileDialogCreateHandle(const AFileDialog: TObject): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: FontDialogCreateHandle
  Params: AFontDialog:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.FontDialogCreateHandle(const AFontDialog: TObject): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: FormCreateHandle
  Params: AForm:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.FormCreateHandle(const AForm: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
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
begin
  Result:=true;
  if Constraints is TSizeConstraints then begin
    SizeConstraints:=TSizeConstraints(Constraints);

    if (SizeConstraints.Control=nil) then exit;

    // TScrollBar
    if SizeConstraints.Control is TScrollBar then begin
      MinWidth := 0;
      MinHeight := 0;
      if TScrollBar(SizeConstraints.Control).Kind=sbHorizontal then begin
        Widget:=GetStyleWidget(lgsHorizontalScrollbar);
        MinHeight:=Widget^.requisition.Height;
      end else begin
        Widget:=GetStyleWidget(lgsVerticalScrollbar);
        MinWidth:=Widget^.requisition.Width;
      end;
      SizeConstraints.SetInterfaceConstraints(MinWidth,MinHeight,
                                              MinWidth,MinHeight);
    end;
    
    // TCustomSplitter
    if SizeConstraints.Control is TCustomSplitter then begin
      MinWidth := 0;
      MinHeight := 0;
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
    end;

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
    ListWidget:=PGtkList(GetWidgetInfo(AWidget, True)^.ImplementationWidget);
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

{------------------------------------------------------------------------------
  Function: GroupBoxCreateHandle
  Params: AGroupBox:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GroupBoxCreateHandle(const AGroupBox: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: HintWindowCreateHandle
  Params: AHintWindow:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.HintWindowCreateHandle(const AHintWindow: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: ImageCreateHandle
  Params: AImage:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ImageCreateHandle(const AImage: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: IntfCreateHandle
  Params: AObject:
          AParams:
  Returns:

  This is a temporary routine until all controls are created directly
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.IntfCreateHandle(const AObject: TObject; const AParams: TCreateParams): THandle;
begin
  Result := CreateComponent(AObject);
end;

{------------------------------------------------------------------------------
  Function: LabelCreateHandle
  Params: ALabel:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.LabelCreateHandle(const ALabel: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: ListBoxCreateHandle
  Params: AListBox:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ListBoxCreateHandle(const AListBox: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: ListViewCreateHandle
  Params: AListView:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ListViewCreateHandle(const AListView: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: MainMenuCreateHandle
  Params: AMainMenu:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.MainMenuCreateHandle(const AMainMenu: TObject): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: MemoCreateHandle
  Params: AMemo:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.MemoCreateHandle(const AMemo: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: MenuItemCreateHandle
  Params: AMenuItem:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.MenuItemCreateHandle(const AMenuItem: TObject): THandle;
begin
  // Your code here
  Result:=0;
end;
{$EndIf}

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
  gtk_widget_set_sensitive(pgtkwidget(AMenuItem.Handle), AMenuItem.Enabled);
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: NotebookCreateHandle
  Params: ANotebook:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.NotebookCreateHandle(const ANotebook: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: OpenFileDialogCreateHandle
  Params: AOpenFileDialog:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.OpenFileDialogCreateHandle(const AOpenFileDialog: TObject): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: PageCreateHandle
  Params: APage:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.PageCreateHandle(const APage: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: PairSplitterCreateHandle
  Params: APairSplitter:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.PairSplitterCreateHandle(const APairSplitter: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: PairSplitterSideCreateHandle
  Params: APairSplitterSide:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.PairSplitterSideCreateHandle(const APairSplitterSide: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: PanelCreateHandle
  Params: APanel:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.PanelCreateHandle(const APanel: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: PopupMenuCreateHandle
  Params: APopupMenu:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.PopupMenuCreateHandle(const APopupMenu: TObject): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: PreviewFileControlCreateHandle
  Params: APreviewFileControl:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.PreviewFileControlCreateHandle(const APreviewFileControl: TObject): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: PreviewFileDialogCreateHandle
  Params: APreviewFileDialog:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.PreviewFileDialogCreateHandle(const APreviewFileDialog: TObject): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: ProgressBarCreateHandle
  Params: AProgressBar:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ProgressBarCreateHandle(const AProgressBar: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: RadioButtonCreateHandle
  Params: ARadioButton:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.RadioButtonCreateHandle(const ARadioButton: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: SaveFileDialogCreateHandle
  Params: ASaveFileDialog:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.SaveFileDialogCreateHandle(const ASaveFileDialog: TObject): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: ScrollBarCreateHandle
  Params: AScrollBar:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ScrollBarCreateHandle(const AScrollBar: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: ScrollBoxCreateHandle
  Params: AScrollBox:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ScrollBoxCreateHandle(const AScrollBox: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: ScrolledWindowCreateHandle
  Params: AScrolledWindow:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ScrolledWindowCreateHandle(const AScrolledWindow: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: SelectDirectoryDialogCreateHandle
  Params: ASelectDirectoryDialog:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.SelectDirectoryDialogCreateHandle(const ASelectDirectoryDialog: TObject): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: SpeedButtonCreateHandle
  Params: ASpeedButton:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.SpeedButtonCreateHandle(const ASpeedButton: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: SpineditCreateHandle
  Params: ASpinEdit:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.SpineditCreateHandle(const ASpinEdit: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: StatusBarCreateHandle
  Params: AStatusBar:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.StatusBarCreateHandle(const AStatusBar: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Procedure: StatusBarPanelUpdate
  Params: StatusBar:
          index:
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.StatusBarPanelUpdate(StatusBar: TObject; Index: integer);
var
  AStatusBar: TStatusBar;
  HBox: PGtkWidget;
  StatusPanelWidget: PGtkWidget;
  BoxChild: PGtkBoxChild;
begin
  //writeln('TGtkWidgetSet.StatusBarPanelUpdate ',HexStr(Cardinal(StatusBar),8),' Index=',Index);
  AStatusBar:=StatusBar as TStatusBar;
  if Index>=0 then begin
    // update one
    HBox:=PGtkWidget(AStatusBar.Handle);
    BoxChild:=PGtkBoxChild(g_list_nth_data(PGtkBox(HBox)^.children,Index));
    if BoxChild=nil then
      RaiseGDBException('TGtkWidgetSet.StatusBarPanelUpdate Index out of bounds');
    StatusPanelWidget:=BoxChild^.Widget;
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
procedure TGtkWidgetSet.StatusBarSetText(StatusBar: TObject; PanelIndex: integer);
begin
  StatusBarPanelUpdate(StatusBar,PanelIndex);
end;

{------------------------------------------------------------------------------
  Procedure: StatusBarUpdate
  Params: StatusBar:
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TGtkWidgetSet.StatusBarUpdate(StatusBar: TObject);
begin
  //writeln('TGtkWidgetSet.StatusBarUpdate ',HexStr(Cardinal(StatusBar),8));
  UpdateStatusBarPanels(StatusBar,PGtkWidget((StatusBar as TStatusBar).Handle));
end;

{------------------------------------------------------------------------------
  Function: ToggleBoxCreateHandle
  Params: AToggleBox:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ToggleBoxCreateHandle(const AToggleBox: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: ToolbarCreateHandle
  Params: AToolbar:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ToolbarCreateHandle(const AToolbar: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: ToolButtonCreateHandle
  Params: AToolButton:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ToolButtonCreateHandle(const AToolButton: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: TrackBarCreateHandle
  Params: ATrackBar:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.TrackBarCreateHandle(const ATrackBar: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: WincontrolCreateHandle
  Params: AFixed:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.WincontrolCreateHandle(const AFixed: TObject; const AParams: TCreateParams): THandle;
begin
  // Your code here
  Result:=0;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

{ =============================================================================

  $Log$
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
