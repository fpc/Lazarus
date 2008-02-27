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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  private
  protected
  public
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
  end;

  { TGtkWSMenu }

  TGtkWSMenu = class(TWSMenu)
  private
  protected
  public
    class function CreateHandle(const AMenu: TMenu): HMENU; override;
  end;

  { TGtkWSMainMenu }

  TGtkWSMainMenu = class(TWSMainMenu)
  private
  protected
  public
  end;

  { TGtkWSPopupMenu }

  TGtkWSPopupMenu = class(TWSPopupMenu)
  private
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  public
    class function CreateHandle(const AMenu: TMenu): HMENU; override;
    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: integer); override;
  end;


implementation

uses Controls;

{ TGtkWSMenuItem }

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

    if GtkWidgetIsA(ParentMenuWidget,GTK_TYPE_MENU_BAR) then begin
      // mainmenu (= a menu bar)
      ContainerMenu:=ParentMenuWidget;
      gtk_menu_bar_insert(ParentMenuWidget,MenuItem, AMenuItem.MenuVisibleIndex);
    end
    else begin
      // menu item

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
  if AMenuItem.Caption='-' then // create separator
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
    {$ifdef GTK2}
    if (OldCheckMenuItemToggleSize=0) then
    begin
      gtk_menu_item_toggle_size_request(GTK_MENU_ITEM(Widget), @OldCheckMenuItemToggleSize);
      OldCheckMenuItemToggleSize := GTK_MENU_ITEM(Widget)^.toggle_size;
    end;
    {$else}
    if (OldCheckMenuItemToggleSize=0) then
      OldCheckMenuItemToggleSize := MENU_ITEM_CLASS(Widget)^.toggle_size;
    {$endif}
    g_signal_connect_after(PGTKObject(Widget), 'toggled',
      TGTKSignalFunc(@GTKCheckMenuToggeledCB), Pointer(AMenuItem));
  end;

  // set attributes (enabled and rightjustify)
  gtk_widget_set_sensitive(Widget,
                           AMenuItem.Enabled and (AMenuItem.Caption<>'-'));
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
                           Enabled and (AMenuItem.Caption<>'-'));
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

{ TGtkWSMenu }

class function  TGtkWSMenu.CreateHandle(const AMenu: TMenu): HMENU;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Box: Pointer;
  ParentForm: TCustomForm;
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
  {$IFDEF GTK2} ForceInScreen: pgboolean; {$ENDIF}
  Point: PPoint); cdecl;
begin
  X^ := Point^.X;
  Y^ := Point^.Y;
end;

function gtkWSPopupDelayedClose(Data: Pointer): gboolean; cdecl;
var
  WidgetInfo: PWidgetInfo absolute Data;
begin
  Result := False;
  if (WidgetInfo<>nil) and (wwiValidQueuedEvent in WidgetInfo^.Flags) then
  begin
    if WidgetInfo^.LCLObject is TPopupMenu then
      TPopupMenu(WidgetInfo^.LCLObject).Close;
  end
  //else DebugLn('No valid popupDelayedClose event');
end;

function gtkWSPopupMenuDeactivate(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
begin
  Include(PWidgetInfo(Data)^.Flags, wwiValidQueuedEvent);
  g_idle_add(@gtkWSPopupDelayedClose, data);
  Result := CallBackDefaultReturn;
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
begin
  ReleaseMouseCapture;
  APoint.X := X;
  APoint.Y := Y;
  
  if (X = Mouse.CursorPos.X) and (Y = Mouse.CursorPos.Y) then
    AProc := nil
  else
    AProc := @GtkWS_Popup;
  gtk_menu_popup(PGtkMenu(APopupMenu.Handle),
                            nil,
                            nil,
                            TGtkMenuPositionFunc(AProc),
                            @APoint,
                            0,
                            0);
  {Displays a menu and makes it available for selection. Applications
  can use this function to display context-sensitive menus, and will
  typically supply NULL for the parent_menu_shell, parent_menu_item,
  func and data parameters.
  The default menu positioning function will position the menu at the
  current pointer position.
  menu :  a GtkMenu.
  parent_menu_shell: the menu shell containing the triggering menu item.
  parent_menu_item: the menu item whose activation triggered the popup.
  func :  a user supplied function used to position the menu.
  data :  user supplied data to be passed to func.
  button :  the button which was pressed to initiate the event.
  activate_time : the time at which the activation event occurred. }
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TMenuItem, TGtkWSMenuItem);
  RegisterWSComponent(TMenu, TGtkWSMenu);
//  RegisterWSComponent(TMainMenu, TGtkWSMainMenu);
  RegisterWSComponent(TPopupMenu, TGtkWSPopupMenu);
////////////////////////////////////////////////////
end.
