{ $Id$}
{
 *****************************************************************************
 *                               GtkWSMenus.pp                               * 
 *                               -------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit GtkWSMenus;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF gtk2}
  glib2, gdk2pixbuf, gdk2, gtk2, Pango,
  {$ELSE}
  glib, gdk, gtk, gdkpixbuf,
  {$ENDIF}
  GtkInt, GtkProc, GtkGlobals, GtkDef, GtkExtra,
  Classes, InterfaceBase, Types, LCLProc, LCLType, WSMenus, WSLCLClasses,
  Graphics, Menus, Forms;

type
  { TGtkWSMenuItem }

  TGtkWSMenuItem = class(TWSMenuItem)
  published
    {$IFDEF GTK1}
    class procedure AttachMenu(const AMenuItem: TMenuItem); override;
    class function CreateHandle(const AMenuItem: TMenuItem): HMENU; override;
    class procedure DestroyHandle(const AMenuItem: TMenuItem); override;
    class procedure SetCaption(const AMenuItem: TMenuItem; const ACaption: string); override;
    class procedure SetShortCut(const AMenuItem: TMenuItem; const OldShortCut, NewShortCut: TShortCut); override;
    class procedure SetVisible(const AMenuItem: TMenuItem; const Visible: boolean); override;
    class function SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean; override;
    class function SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean; override;
    class function SetRadioItem(const AMenuItem: TMenuItem; const RadioItem: boolean): boolean; override;
    class function SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean; override;
    class procedure UpdateMenuIcon(const AMenuItem: TMenuItem; const HasIcon: Boolean; const AIcon: TBitmap); override;
    {$ENDIF}
  end;

  { TGtkWSMenu }

  TGtkWSMenu = class(TWSMenu)
  published
    class function CreateHandle(const AMenu: TMenu): HMENU; override;
  end;

  { TGtkWSMainMenu }

  TGtkWSMainMenu = class(TWSMainMenu)
  published
  end;

  { TGtkWSPopupMenu }

  TGtkWSPopupMenu = class(TWSPopupMenu)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const AMenu: TMenu): HMENU; override;
    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: integer); override;
  end;


implementation

uses Controls;

{ TGtkWSMenuItem }

{$IFDEF GTK1}
class procedure TGtkWSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
var
  //AccelKey: Integer;
  //AccelGroup: PGTKAccelGroup;
  MenuItem, ParentMenuWidget, ContainerMenu: PGtkWidget;

  procedure SetContainerMenuToggleSize;
  var MenuClass: PGtkWidgetClass;
  begin
    if GtkWidgetIsA(ContainerMenu,GTK_TYPE_MENU) then begin
      MenuClass:=GTK_WIDGET_CLASS(gtk_object_get_class(ContainerMenu));
      if OldMenuSizeRequestProc=nil then begin
        OldMenuSizeRequestProc:=MenuClass^.size_request;
      end;
      MenuClass^.size_request:=@MenuSizeRequest;
    end;
  end;

begin
  //DebugLn('TGtkWidgetSet.AttachMenu START ',AMenuItem.Name,':',AMenuItem.ClassName,' Parent=',AMenuItem.Parent.Name,':',AMenuItem.Parent.ClassName);
  with AMenuItem do
  begin
    MenuItem := PGtkWidget(Handle);
    if MenuItem=nil then
      RaiseGDBException('TGtkWidgetSet.AttachMenu Handle=0');
    ParentMenuWidget := PGtkWidget(Parent.Handle);
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
        ContainerMenu := PGtkWidget(gtk_object_get_data(PGtkObject(ParentMenuWidget),
                                    'ContainerMenu')); // find the menu container

      if ContainerMenu = nil then
      begin
        if (GetParentMenu is TPopupMenu) and (Parent.Parent=nil) then
        begin
          ContainerMenu := PGtkWidget(GetParentMenu.Handle);
          gtk_object_set_data(PGtkObject(ContainerMenu), 'ContainerMenu',
                              ContainerMenu);
        end else
        begin
          ContainerMenu := gtk_menu_new;
          gtk_object_set_data(PGtkObject(ParentMenuWidget), 'ContainerMenu',
                              ContainerMenu);
          gtk_menu_item_set_submenu(PGTKMenuItem(ParentMenuWidget), ContainerMenu);
        end;
      end;
      gtk_menu_insert(ContainerMenu, MenuItem, AMenuItem.MenuVisibleIndex);
    end;

    SetContainerMenuToggleSize;

    if GtkWidgetIsA(MenuItem, GTK_TYPE_RADIO_MENU_ITEM) then
      TGtkWidgetSet(WidgetSet).RegroupMenuItem(HMENU(PtrUInt(MenuItem)),GroupIndex);
  end;
  //DebugLn('TGtkWidgetSet.AttachMenu END ',AMenuItem.Name,':',AMenuItem.ClassName);
end;

class function  TGtkWSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  // create the menuitem widget (normal, check or radio)
  if AMenuItem.Caption = cLineCaption then // create separator
    Widget := gtk_menu_item_new
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
    // set 'ShowAlwaysCheckable'
    gtk_check_menu_item_set_show_toggle(PGtkCheckMenuItem(Widget),
      AMenuItem.ShowAlwaysCheckable);
    // set 'Checked'
    gtk_check_menu_item_set_active(PGtkCheckMenuItem(Widget),
      AMenuItem.Checked);

    if (OldCheckMenuItemToggleSize=0) then
      OldCheckMenuItemToggleSize := MENU_ITEM_CLASS(Widget)^.toggle_size;

    g_signal_connect_after(PGTKObject(Widget), 'toggled',
      TGTKSignalFunc(@GTKCheckMenuToggeledCB), Pointer(AMenuItem));
  end;

  // set attributes (enabled and rightjustify)
  gtk_widget_set_sensitive(Widget,
                           AMenuItem.Enabled and (AMenuItem.Caption <> cLineCaption));
  if AMenuItem.RightJustify then
    gtk_menu_item_right_justify(PGtkMenuItem(Widget));

  // create the hbox containing the label and the icon
  UpdateInnerMenuItem(AMenuItem, Widget);

  // connect activate signal (i.e. clicked)
  g_signal_connect(PGTKObject(Widget), 'activate',
                   TGTKSignalFunc(@gtkactivateCB), AMenuItem);

  gtk_widget_show(Widget);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AMenuItem));
  {$ENDIF}
  Result := HMENU(PtrUInt(Widget));
end;

class procedure TGtkWSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
begin
  { TODO: cleanup }
  TGtkWidgetSet(WidgetSet).DestroyLCLComponent(AMenuItem);
end;

class procedure TGtkWSMenuItem.SetCaption(const AMenuItem: TMenuItem;
  const ACaption: string);
var
  MenuItemWidget: PGtkWidget;
begin
  if not WSCheckMenuItem(AMenuItem, 'SetCaption') then
    Exit;
  MenuItemWidget:=PGtkWidget(AMenuItem.Handle);
  UpdateInnerMenuItem(AMenuItem,MenuItemWidget);
  gtk_widget_set_sensitive(PGtkWidget(AMenuItem.Handle),
                           AMenuItem.Enabled and (ACaption <> cLineCaption));
end;

class procedure TGtkWSMenuItem.SetShortCut(const AMenuItem: TMenuItem;
  const OldShortCut, NewShortCut: TShortCut);
begin
  if not WSCheckMenuItem(AMenuItem, 'SetShortCut') then
    Exit;
  //DebugLn(['TGtkWSMenuItem.SetShortCut ',dbgsName(AMenuItem),' ',ShortCutToText(NewShortCut)]);
  UpdateInnerMenuItem(AMenuItem, PGTKWidget(AMenuItem.Handle), NewShortCut);
end;

class procedure TGtkWSMenuItem.SetVisible(const AMenuItem: TMenuItem;
  const Visible: boolean);
var
  MenuItemWidget: PGtkWidget;
begin
  if not WSCheckMenuItem(AMenuItem, 'SetVisible') then
    Exit;
  MenuItemWidget := PGtkWidget(AMenuItem.Handle);
  if gtk_widget_visible(MenuItemWidget) = Visible then
    Exit;
  if Visible then
    gtk_widget_show(MenuItemWidget)
  else
    gtk_widget_hide(MenuItemWidget);
end;

class function TGtkWSMenuItem.SetCheck(const AMenuItem: TMenuItem;
  const Checked: boolean): boolean;
var
  IsRadio: Boolean;
  Group: PGSList;
  Item: Pointer;
begin
  if not WSCheckMenuItem(AMenuItem, 'SetCheck') then
    Exit;
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

class function TGtkWSMenuItem.SetEnable(const AMenuItem: TMenuItem;
  const Enabled: boolean): boolean;
begin
  Result := False;
  if not WSCheckMenuItem(AMenuItem, 'SetEnable') then
    Exit;
  gtk_widget_set_sensitive(PGtkWidget(AMenuItem.Handle),
                           Enabled and (AMenuItem.Caption <> cLineCaption));
  Result := True;
end;

class function TGtkWSMenuItem.SetRadioItem(const AMenuItem: TMenuItem;
  const RadioItem: boolean): boolean;
begin
  AMenuItem.RecreateHandle;
  Result := True;
end;

class function TGtkWSMenuItem.SetRightJustify(const AMenuItem: TMenuItem;
  const Justified: boolean): boolean;
var
  MenuItemWidget: PGtkMenuItem;
begin
  Result := False;
  if not WSCheckMenuItem(AMenuItem, 'SetRightJustify') then
    Exit;
  MenuItemWidget := PGtkMenuItem(AMenuItem.Handle);
  gtk_menu_item_set_right_justified(MenuItemWidget, Justified);
  gtk_widget_queue_resize(GTK_WIDGET(MenuItemWidget));
  Result := True;
end;

class procedure TGtkWSMenuItem.UpdateMenuIcon(const AMenuItem: TMenuItem;
  const HasIcon: Boolean; const AIcon: TBitmap);
begin
  if not WSCheckMenuItem(AMenuItem, 'UpdateMenuIcon') then
    Exit;
  // TODO
end;
{$ENDIF}

{ TGtkWSMenu }

class function TGtkWSMenu.CreateHandle(const AMenu: TMenu): HMENU;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Box: Pointer;
  ParentForm: TCustomForm;
{$ifdef GTK2}
const
  MenuDirection : array[Boolean] of Longint = (
    GTK_PACK_DIRECTION_LTR,
    GTK_PACK_DIRECTION_RTL);
{$endif}
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
    Box := PGTKBin(ParentForm.Handle)^.Child;
    gtk_box_pack_start(Box, Widget, False, False, 0);
  end;

  {$ifdef GTK2}
  gtk_menu_bar_set_pack_direction(PGtkMenuBar(Widget), MenuDirection[AMenu.UseRightToLeftAlignment]);
  {$endif}

  gtk_widget_show(Widget);

  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AMenu));
  {$ENDIF}
  Result := THandle(PtrUInt(Widget));
  WidgetInfo := CreateWidgetInfo(Widget);
  WidgetInfo^.LCLObject := AMenu;
  // no callbacks for main menu
end;

{ TGtkWSPopupMenu }
procedure GtkWS_Popup(menu: PGtkMenu; X, Y: pgint;
  {$IFDEF GTK2} push_in: pgboolean; {$ENDIF}
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
  if data <> nil then
    g_idle_add(@gtkWSPopupDelayedClose, Pointer(PWidgetInfo(data)^.LCLObject));
end;

class procedure TGtkWSPopupMenu.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  g_signal_connect_after(PGtkObject(AGtkWidget), 'deactivate',
    gtk_signal_func(@gtkWSPopupMenuDeactivate), AWidgetInfo);
end;

class function TGtkWSPopupMenu.CreateHandle(const AMenu: TMenu): HMENU;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  Widget := gtk_menu_new;
  Result := HMENU(PtrUInt(Widget));
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(Sender));
  {$ENDIF}
  WidgetInfo := CreateWidgetInfo(Widget);
  WidgetInfo^.LCLObject := AMenu;
  SetCallbacks(Widget, WidgetInfo);
end;

class procedure TGtkWSPopupMenu.Popup(const APopupMenu: TPopupMenu;
  const X, Y: integer);
var
  APoint: TPoint;
  AProc: Pointer;
  MenuWidget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  ReleaseMouseCapture;
  APoint.X := X;
  APoint.Y := Y;
  AProc := @GtkWS_Popup;

  MenuWidget := PGtkWidget(APopupMenu.Handle);
  WidgetInfo := GetWidgetInfo(MenuWidget);
  WidgetInfo^.UserData := @APoint;
  WidgetInfo^.DataOwner := False;
  // MenuWidget can be either GtkMenu or GtkMenuItem submenu
  if GTK_IS_MENU_ITEM(MenuWidget) then
  {$ifdef gtk1}
    MenuWidget := gtk_object_get_data(PGtkObject(MenuWidget), 'ContainerMenu');
  {$else}
    MenuWidget := gtk_menu_item_get_submenu(PGtkMenuItem(MenuWidget));
  {$endif}
  gtk_menu_popup(PGtkMenu(MenuWidget), nil, nil, TGtkMenuPositionFunc(AProc),
                 WidgetInfo, 0,
                 {$ifdef gtk1}
                   gdk_event_get_time(gtk_get_current_event)
                 {$else}
                   gtk_get_current_event_time()
                 {$endif}
                 );
end;

end.
