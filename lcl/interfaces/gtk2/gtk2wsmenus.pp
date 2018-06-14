{ $Id$}
{
 *****************************************************************************
 *                              Gtk2WSMenus.pp                               *
 *                              --------------                               *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Gtk2WSMenus;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  Classes, Types, glib2, gdk2, gtk2,
  // LazUtils
  LazTracer,
  // LCL
  Gtk2Int, Gtk2Proc, Gtk2Globals, Gtk2Def, Gtk2Extra,
  LCLType, LCLIntf, InterfaceBase, WSMenus, LMessages, Graphics, Menus, Forms;

type

  { TGtk2WSMenuItem }

  TGtk2WSMenuItem = class(TWSMenuItem)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
  published
    class procedure AttachMenu(const AMenuItem: TMenuItem); override;
    class function CreateHandle(const AMenuItem: TMenuItem): HMENU; override;
    class procedure DestroyHandle(const AMenuItem: TMenuItem); override;
    class procedure SetCaption(const AMenuItem: TMenuItem; const ACaption: string); override;
    class procedure SetShortCut(const AMenuItem: TMenuItem; const ShortCutK1, ShortCutK2: TShortCut); override;
    class procedure SetVisible(const AMenuItem: TMenuItem; const Visible: boolean); override;
    class function SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean; override;
    class function SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean; override;
    class function SetRadioItem(const AMenuItem: TMenuItem; const {%H-}RadioItem: boolean): boolean; override;
    class function SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean; override;
    class procedure UpdateMenuIcon(const AMenuItem: TMenuItem; const HasIcon: Boolean; const {%H-}AIcon: TBitmap); override;
  end;

  { TGtk2WSMenu }

  TGtk2WSMenu = class(TWSMenu)
  published
    class function CreateHandle(const AMenu: TMenu): HMENU; override;
    class procedure SetBiDiMode(const AMenu: TMenu; UseRightToLeftAlign, {%H-}UseRightToLeftReading : Boolean); override;
  end;

  { TGtk2WSMainMenu }

  TGtk2WSMainMenu = class(TWSMainMenu)
  published
  end;

  { TGtk2WSPopupMenu }

  TGtk2WSPopupMenu = class(TWSPopupMenu)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const AMenu: TMenu): HMENU; override;
    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: integer); override;
  end;


implementation

{$I gtk2defines.inc}

var
  MenuWidget: PGtkWidget = nil;

function Gtk2MenuItemButtonPress(widget: PGtkWidget; event: PGdkEventButton;
 {%H-}user_data: gpointer): gboolean; cdecl;
var
  Parent: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  Result := False;
  if (event^._type = GDK_BUTTON_PRESS) then
  begin
    Parent := gtk_widget_get_parent(Widget);
    if (Parent <> nil) and GTK_IS_MENU_BAR(Parent) then
    begin
      if (gtk_menu_item_get_submenu(PGtkMenuItem(Widget)) = nil) then
      begin
        WidgetInfo := GetWidgetInfo(Widget);
        if Assigned(TMenuItem(WidgetInfo^.LCLObject).OnClick) then
        begin
          gtk_menu_item_activate(PGtkMenuItem(Widget));
          // must be true because of issue #22616
          Result := True;
        end;
      end;
    end;
  end;
end;

function Gtk2MenuItemActivate(widget: PGtkMenuItem; data: gPointer) : GBoolean; cdecl;
var
  Mess: TLMActivate;
  LCLMenuItem: TMenuItem;
begin
  Result:= True;
  {$IFDEF EventTrace}
  EventTrace('activate', data);
  {$ENDIF}

  ResetDefaultIMContext;

  if LockOnChange(PgtkObject(Widget),0) > 0 then Exit;

  LCLMenuItem := TMenuItem(Data);

  // the gtk fires activate for radio buttons when unchecking them
  // the LCL expects only uncheck
  if LCLMenuItem.RadioItem
  and GtkWidgetIsA(PGtkWidget(Widget), GTK_TYPE_CHECK_MENU_ITEM)
  and (not gtk_check_menu_item_get_active(PGTKCheckMenuItem(Widget))) then Exit;

  FillChar(Mess{%H-}, SizeOf(Mess), #0);
  Mess.Msg := LM_ACTIVATE;
  Mess.Active := WA_ACTIVE;
  Mess.Minimized := False;
  Mess.ActiveWindow := 0;
  Mess.Result := 0;
  DeliverMessage(Data, Mess);

  Result := CallBackDefaultReturn;
end;

function Gtk2MenuItemToggled(AMenuItem: PGTKCheckMenuItem;
                             AData: gPointer): GBoolean; cdecl;
var
  LCLMenuItem: TMenuItem;
  Mess: TLMessage;
  b: Boolean;
  w: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  Result := CallBackDefaultReturn;
  {$IFDEF EventTrace}
  EventTrace('toggled', AData);
  {$ENDIF}
  if LockOnChange(PgtkObject(AMenuItem),0) > 0 then Exit;

  LCLMenuItem := TMenuItem(AData);

  if (csDesigning in LCLMenuItem.ComponentState) then
    exit;

  w := gtk_get_event_widget(gtk_get_current_event);

  if not GTK_IS_RADIO_MENU_ITEM(w) then
    exit;

  b := gtk_check_menu_item_get_active(AMenuItem);

  if not LCLMenuItem.Checked then
    g_signal_stop_emission_by_name(AMenuItem, 'toggled')
  else
    g_signal_stop_emission_by_name(AMenuItem, 'activate');

  if b <> LCLMenuItem.Checked then
    gtk_check_menu_item_set_active(AMenuItem, LCLMenuItem.Checked);

  {we must trigger OnClick() somehow, since we stopped signals}
  if b and (w <> nil) and (w <> PGtkWidget(AMenuItem)) then
  begin
    WidgetInfo := GetWidgetInfo(w);
    FillChar(Mess{%H-},SizeOf(Mess),#0);
    Mess.Msg := LM_ACTIVATE;
    WidgetInfo^.LCLObject.Dispatch(Mess);
  end;
end;

function Gtk2MenuItemSelect({%H-}item: PGtkMenuItem; AMenuItem: gPointer): GBoolean; cdecl;
begin
  TMenuItem(AMenuItem).IntfDoSelect;
  Result := CallBackDefaultReturn;
end;

procedure Gtk2MenuItemToggleSizeRequest(AMenuItem: PGtkMenuItem; requisition: Pgint; LCLItem: TMenuItem); cdecl;
var
  spacing: guint;
  IconWidth: Integer;
  DC: HDC;
begin
  if LCLItem.HasIcon then
  begin
    DC := Widgetset.GetDC(HWND({%H-}PtrUInt(AMenuItem)));
    IconWidth := LCLItem.GetIconSize(DC).X;
    WidgetSet.ReleaseDC(HWND({%H-}PtrUInt(AMenuItem)), DC);
    if IconWidth > 0 then
    begin
      gtk_widget_style_get(PGtkWidget(AMenuItem), 'toggle-spacing', [@spacing, nil]);
      requisition^ := IconWidth + spacing;
    end
    else
      requisition^ := 0;
  end
  else
    GTK_MENU_ITEM_GET_CLASS(AMenuItem)^.toggle_size_request(AMenuItem, requisition);
end;

procedure Gtk2MenuItemSizeRequest(AMenuItem: PGtkMenuItem; requisition: PGtkRequisition; LCLItem: TMenuItem); cdecl;
var
  IconHeight: Integer;
  DC: HDC;
begin
  GTK_WIDGET_GET_CLASS(AMenuItem)^.size_request(PGtkWidget(AMenuItem), requisition);
  DC := Widgetset.GetDC(HWND({%H-}PtrUInt(AMenuItem)));
  IconHeight := LCLItem.GetIconSize(DC).Y;
  Widgetset.ReleaseDC(HWND({%H-}PtrUInt(AMenuItem)), DC);
  if requisition^.height < IconHeight then
    requisition^.height := IconHeight;
end;

function Gtk2MenuItemDeselect({%H-}item: Pointer; {%H-}AMenuItem: TMenuItem): GBoolean; cdecl;
begin
  Application.Hint := '';
  Result := CallBackDefaultReturn;
end;

{ TGtk2WSMenuItem }

class procedure TGtk2WSMenuItem.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  // connect activate signal (i.e. clicked)
  {button-press-event is needed by root menu items which have not
  submenu, but OnClick() is assigned - fix for #15986 }
  g_signal_connect_after(PGTKObject(AGtkWidget), 'button-press-event',
    TGTKSignalFunc(@Gtk2MenuItemButtonPress), AWidgetInfo^.LCLObject);
  g_signal_connect(PGTKObject(AGtkWidget), 'activate',
                   TGTKSignalFunc(@Gtk2MenuItemActivate), AWidgetInfo^.LCLObject);
  g_signal_connect(PGTKObject(AGtkWidget), 'select',
    TGTKSignalFunc(@Gtk2MenuItemSelect), AWidgetInfo^.LCLObject);
  g_signal_connect(PGTKObject(AGtkWidget), 'deselect',
    TGTKSignalFunc(@Gtk2MenuItemDeselect), AWidgetInfo^.LCLObject);
  g_signal_connect(PGTKObject(AGtkWidget), 'toggle-size-request',
    TGTKSignalFunc(@Gtk2MenuItemToggleSizeRequest), AWidgetInfo^.LCLObject);
  g_signal_connect(PGTKObject(AGtkWidget), 'size-request',
    TGTKSignalFunc(@Gtk2MenuItemSizeRequest), AWidgetInfo^.LCLObject);
end;

class procedure TGtk2WSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
var
  MenuItem, ParentMenuWidget, ContainerMenu: PGtkWidget;
begin
  with AMenuItem do
  begin
    MenuItem := {%H-}PGtkWidget(Handle);
    if MenuItem=nil then
      RaiseGDBException('TGtkWidgetSet.AttachMenu Handle=0');
    ParentMenuWidget := {%H-}PGtkWidget(Parent.Handle);
    if ParentMenuWidget=nil then
      RaiseGDBException('TGtkWidgetSet.AttachMenu ParentMenuWidget=nil');

    if GTK_IS_MENU_BAR(ParentMenuWidget) then
    begin
      // mainmenu (= a menu bar)
      ContainerMenu := ParentMenuWidget;
      gtk_menu_bar_insert(ParentMenuWidget, MenuItem, AMenuItem.MenuVisibleIndex);
    end
    else
    begin
      // if it is a menu
      if GTK_IS_MENU(ParentMenuWidget) then
        ContainerMenu := ParentMenuWidget
      else // menu item
        ContainerMenu := PGtkWidget(g_object_get_data(PGObject(ParentMenuWidget),
                                    'ContainerMenu')); // find the menu container

      if ContainerMenu = nil then
      begin
        if (GetParentMenu is TPopupMenu) and (Parent.Parent=nil) then
        begin
          ContainerMenu := {%H-}PGtkWidget(GetParentMenu.Handle);
          g_object_set_data(PGObject(ContainerMenu), 'ContainerMenu',
                              ContainerMenu);
        end else
        begin
          ContainerMenu := gtk_menu_new;
          g_object_set_data(PGObject(ParentMenuWidget), 'ContainerMenu',
                              ContainerMenu);
          gtk_menu_item_set_submenu(PGTKMenuItem(ParentMenuWidget), ContainerMenu);
        end;
      end;
      gtk_menu_insert(ContainerMenu, MenuItem, AMenuItem.MenuVisibleIndex);
    end;

    if GtkWidgetIsA(MenuItem, GTK_TYPE_RADIO_MENU_ITEM) then
      TGtk2WidgetSet(WidgetSet).RegroupMenuItem(HMENU({%H-}PtrUInt(MenuItem)), GroupIndex);
  end;
end;

class function TGtk2WSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  // create the menuitem widget (normal, check or radio)
  if AMenuItem.Caption = cLineCaption then // create separator
    Widget := gtk_separator_menu_item_new
  else
  if AMenuItem.RadioItem and not AMenuItem.HasIcon then
    Widget := gtk_radio_menu_item_new(nil)
  else
  if AMenuItem.IsCheckItem or AMenuItem.HasIcon then
    Widget := gtk_check_menu_item_new
  else
    Widget := gtk_menu_item_new;

  WidgetInfo := CreateWidgetInfo(Widget);
  WidgetInfo^.LCLObject := AMenuItem;

  if GtkWidgetIsA(Widget, GTK_TYPE_CHECK_MENU_ITEM) then
  begin
    // check or radio
    // set 'ShowAlwaysCheckable'
    gtk_check_menu_item_set_show_toggle(PGtkCheckMenuItem(Widget),
      AMenuItem.ShowAlwaysCheckable);
    // set 'Checked'
    gtk_check_menu_item_set_active(PGtkCheckMenuItem(Widget),
      AMenuItem.Checked);

    g_signal_connect(PGTKObject(Widget), 'toggled',
      TGTKSignalFunc(@Gtk2MenuItemToggled), Pointer(AMenuItem));
  end;

  // set attributes (enabled and rightjustify)
  gtk_widget_set_sensitive(Widget, AMenuItem.Enabled);
  if AMenuItem.RightJustify then
    gtk_menu_item_right_justify(PGtkMenuItem(Widget));

  // create the hbox containing the label and the icon
  UpdateInnerMenuItem(AMenuItem, Widget);

  SetCallbacks(Widget, WidgetInfo);

  gtk_widget_show(Widget);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AMenuItem));
  {$ENDIF}
  Result := HMENU({%H-}PtrUInt(Widget));
end;

class procedure TGtk2WSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
begin
  { TODO: cleanup }
  TGtk2WidgetSet(WidgetSet).DestroyLCLComponent(AMenuItem);
end;

class procedure TGtk2WSMenuItem.SetCaption(const AMenuItem: TMenuItem;
  const ACaption: string);
var
  MenuItemWidget: PGtkWidget;
begin
  if not WSCheckMenuItem(AMenuItem, 'SetCaption') then
    Exit;
  if gtk_is_separator_menu_item({%H-}PGTKWidget(AMenuItem.Handle)) Or (ACaption = cLineCaption) then
   AMenuItem.RecreateHandle
  else
   begin
    MenuItemWidget:={%H-}PGtkWidget(AMenuItem.Handle);
    UpdateInnerMenuItem(AMenuItem,MenuItemWidget);
    gtk_widget_set_sensitive({%H-}PGtkWidget(AMenuItem.Handle), AMenuItem.Enabled);
   end;
end;

class procedure TGtk2WSMenuItem.SetShortCut(const AMenuItem: TMenuItem;
  const ShortCutK1, ShortCutK2: TShortCut);
//var
  //MenuWidget: PGtkMenuItem;
  //accel_path: String;
  //CurKey: Word;
  //CurShift: TShiftState;
begin
  if not WSCheckMenuItem(AMenuItem, 'SetShortCut') then  Exit;

  // Temporary: At least it writes the names of the shortcuts
  UpdateInnerMenuItem(AMenuItem, {%H-}PGTKWidget(AMenuItem.Handle), ShortCutK1, ShortCutK2);

{  // Gets the inner widgets. They should already be created by now
  MenuWidget := PGtkMenuItem(AMenuItem.Handle);
  if (MenuWidget=nil) then Exit;
  // Converts the shortcut to a gtk friendly format and sets it
  ShortCutToKey(NewShortCut, CurKey, CurShift);
  accel_path := 'LCLApp/Menu/' + GetAcceleratorString(CurKey, CurShift);
  gtk_accel_map_add_entry(accel_path, CurKey, ShiftToGdkModifierType);
  gtk_menu_item_set_accel_path(); }
end;

class procedure TGtk2WSMenuItem.SetVisible(const AMenuItem: TMenuItem;
  const Visible: boolean);
var
  MenuItemWidget: PGtkWidget;
begin
  if not WSCheckMenuItem(AMenuItem, 'SetVisible') then
    Exit;
  MenuItemWidget := {%H-}PGtkWidget(AMenuItem.Handle);
  if gtk_widget_visible(MenuItemWidget) = Visible then
    Exit;
  if Visible then
    gtk_widget_show(MenuItemWidget)
  else
    gtk_widget_hide(MenuItemWidget);
end;

class function TGtk2WSMenuItem.SetCheck(const AMenuItem: TMenuItem;
  const Checked: boolean): boolean;
var
  IsRadio: Boolean;
  Group: PGSList;
  Item: Pointer;
begin
  Result:=false;
  if not WSCheckMenuItem(AMenuItem, 'SetCheck') then
    Exit;
  Item := {%H-}Pointer(AMenuItem.Handle);
  IsRadio := gtk_is_radio_menu_item(Item);
  if IsRadio or gtk_is_check_menu_item(Item)
  then begin
    if IsRadio
    then begin
      Group := gtk_radio_menu_item_group(Item);
      LockRadioGroupOnChange(Group, +1);
    end
    else LockOnChange(Item, +1);
    gtk_check_menu_item_set_active(Item, Checked);
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

class function TGtk2WSMenuItem.SetEnable(const AMenuItem: TMenuItem;
  const Enabled: boolean): boolean;
begin
  Result := False;
  if not WSCheckMenuItem(AMenuItem, 'SetEnable') then
    Exit;
  gtk_widget_set_sensitive({%H-}PGtkWidget(AMenuItem.Handle), Enabled);
  Result := True;
end;

class function TGtk2WSMenuItem.SetRadioItem(const AMenuItem: TMenuItem;
  const RadioItem: boolean): boolean;
begin
  AMenuItem.RecreateHandle;
  Result := True;
end;

class function TGtk2WSMenuItem.SetRightJustify(const AMenuItem: TMenuItem;
  const Justified: boolean): boolean;
var
  MenuItemWidget: PGtkMenuItem;
begin
  Result := False;
  if not WSCheckMenuItem(AMenuItem, 'SetRightJustify') then
    Exit;
  MenuItemWidget := {%H-}PGtkMenuItem(AMenuItem.Handle);
  gtk_menu_item_set_right_justified(MenuItemWidget, Justified);
  gtk_widget_queue_resize(GTK_WIDGET(MenuItemWidget));
  Result := True;
end;

class procedure TGtk2WSMenuItem.UpdateMenuIcon(const AMenuItem: TMenuItem;
  const HasIcon: Boolean; const AIcon: TBitmap);
begin
  if not WSCheckMenuItem(AMenuItem, 'UpdateMenuIcon') then
    Exit;
  if gtk_is_check_menu_item({%H-}Pointer(AMenuItem.Handle)) <> HasIcon then
    AMenuItem.RecreateHandle;
end;

{ TGtk2WSMenu }

class function TGtk2WSMenu.CreateHandle(const AMenu: TMenu): HMENU;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Box: Pointer;
  ParentForm: TCustomForm;
const
  MenuDirection : array[Boolean] of Longint = (
    GTK_PACK_DIRECTION_LTR,
    GTK_PACK_DIRECTION_RTL);
begin
  Widget := gtk_menu_bar_new();
  // get the VBox, the form has one child, a VBox
  ParentForm := TCustomForm(AMenu.Parent);
  if (ParentForm=nil) or (not (ParentForm is TCustomForm)) then
    RaiseGDBException('MainMenu without form');
  if ParentForm.Menu <> AMenu then
    RaiseGDBException('Form already has a MainMenu');
  if ParentForm.HandleAllocated then
  begin
    Box := {%H-}PGTKBin(ParentForm.Handle)^.Child;
    gtk_box_pack_start(Box, Widget, False, False, 0);
  end;

  gtk_menu_bar_set_pack_direction(PGtkMenuBar(Widget), MenuDirection[AMenu.UseRightToLeftAlignment]);
  gtk_widget_show(Widget);

  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AMenu));
  {$ENDIF}
  Result := THandle({%H-}PtrUInt(Widget));
  WidgetInfo := CreateWidgetInfo(Widget);
  WidgetInfo^.LCLObject := AMenu;
  // no callbacks for main menu
end;

class procedure TGtk2WSMenu.SetBiDiMode(const AMenu : TMenu;
  UseRightToLeftAlign, UseRightToLeftReading : Boolean);
const
  WidgetDirection : array[boolean] of longint = (GTK_TEXT_DIR_LTR, GTK_TEXT_DIR_RTL);
{$ifdef GTK_2_8}
const
  MenuDirection : array[Boolean] of Longint = (
    GTK_PACK_DIRECTION_LTR,
    GTK_PACK_DIRECTION_RTL);
{$endif}
  procedure Switch(AMenuItem: TMenuItem; Flip: Boolean);
  var
    i: Integer;
  begin
    if Flip then
    begin
      if AMenuItem.HandleAllocated then begin
        gtk_widget_set_direction({%H-}PGtkWidget(AMenuItem.Handle), WidgetDirection[UseRightToLeftAlign]);
        UpdateInnerMenuItem(AMenuItem, {%H-}PGtkWidget(AMenuItem.Handle));
      end;
    end;
    for i := 0 to AMenuItem.Count -1 do
      Switch(AMenuItem[i], True);
  end;
begin
  {$ifdef GTK_2_8}
    gtk_menu_bar_set_pack_direction({%H-}PGtkMenuBar(AMenu.Handle), MenuDirection[UseRightToLeftAlign]);
    gtk_menu_bar_set_child_pack_direction({%H-}PGtkMenuBar(AMenu.Handle), MenuDirection[UseRightToLeftAlign]);
  {$endif}
  //gtk_widget_set_direction(PGtkWidget(AMenu.Handle), WidgetDirection[UseRightToLeftAlign]);
  Switch(AMenu.Items, False);
end;

{ TGtk2WSPopupMenu }

procedure GtkWS_Popup(menu: PGtkMenu; X, Y: pgint; {%H-}push_in: pgboolean;
  WidgetInfo: PWidgetInfo); cdecl;
var
  Requisition: TGtkRequisition;
  Alignment: TPopupAlignment;
  ScreenHeight: gint;
begin
  X^ := PPoint(WidgetInfo^.UserData)^.X;
  Y^ := PPoint(WidgetInfo^.UserData)^.Y;

  if WidgetInfo^.LCLObject is TPopupMenu then
  begin
    // make menu to fit the screen vertically
    gtk_widget_size_request(PGtkWidget(menu), @Requisition);
    ScreenHeight := gdk_screen_height();
    if Y^ + Requisition.height > ScreenHeight then
    begin
      Y^ := ScreenHeight - Requisition.height;
      if Y^ < 0 then Y^ := 0;
    end;

    // get actual alignment
    Alignment := TPopupMenu(WidgetInfo^.LCLObject).Alignment;
    if TPopupMenu(WidgetInfo^.LCLObject).UseRightToLeftAlignment then
    begin
      if Alignment = paLeft then
        Alignment := paRight
      else
      if Alignment = paRight then
        Alignment := paLeft;
    end;

    case Alignment of
      paCenter: X^ := X^ - Requisition.width div 2;
      paRight: X^ := X^ - Requisition.width;
    end;
  end;
end;

function gtkWSPopupDelayedClose(Data: Pointer): gboolean; cdecl;
var
  PopupMenu: TPopupMenu absolute data;
begin
  Result := False;
  if PopupMenu is TPopupMenu then
    PopupMenu.Close;
end;

procedure gtkWSPopupMenuDeactivate(widget: PGtkWidget; data: gPointer); cdecl;
begin
  if widget = MenuWidget then
    MenuWidget := nil;
  if data <> nil then
    g_idle_add(@gtkWSPopupDelayedClose, Pointer(PWidgetInfo(data)^.LCLObject));
end;


class procedure TGtk2WSPopupMenu.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  g_signal_connect_after(PGtkObject(AGtkWidget), 'deactivate',
    gtk_signal_func(@gtkWSPopupMenuDeactivate), AWidgetInfo);
end;

class function TGtk2WSPopupMenu.CreateHandle(const AMenu: TMenu): HMENU;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  Widget := gtk_menu_new;
  Result := HMENU({%H-}PtrUInt(Widget));
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AMenu));
  {$ENDIF}
  WidgetInfo := CreateWidgetInfo(Widget);
  WidgetInfo^.LCLObject := AMenu;
  SetCallbacks(Widget, WidgetInfo);
end;

class procedure TGtk2WSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X,
  Y: integer);
var
  APoint: TPoint;
  AProc: Pointer;
  WidgetInfo: PWidgetInfo;
begin
  if MenuWidget<>nil then //cannot popup when another popup menu is visible
    Exit;

  ReleaseMouseCapture;
  APoint.X := X;
  APoint.Y := Y;
  AProc := @GtkWS_Popup;

  MenuWidget := {%H-}PGtkWidget(APopupMenu.Handle);
  WidgetInfo := GetWidgetInfo(MenuWidget);
  WidgetInfo^.UserData := @APoint;
  WidgetInfo^.DataOwner := False;
  // MenuWidget can be either GtkMenu or GtkMenuItem submenu
  if GTK_IS_MENU_ITEM(MenuWidget) then
    MenuWidget := gtk_menu_item_get_submenu(PGtkMenuItem(MenuWidget));
  gtk_menu_popup(PGtkMenu(MenuWidget), nil, nil, TGtkMenuPositionFunc(AProc),
                 WidgetInfo, 0, gtk_get_current_event_time());
  repeat
    try
      WidgetSet.AppProcessMessages; // process all events
    except
      if Application.CaptureExceptions then
        Application.HandleException(APopupMenu)
      else
        raise;
    end;
    if Application.Terminated or not Assigned(MenuWidget) then
      break;
    Application.Idle(true);
  until False;
end;

end.
