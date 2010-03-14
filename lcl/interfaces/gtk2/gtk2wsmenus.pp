{ $Id$}
{
 *****************************************************************************
 *                              Gtk2WSMenus.pp                               * 
 *                              --------------                               * 
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
unit Gtk2WSMenus;

{$mode objfpc}{$H+}

interface

uses
  glib2, gdk2pixbuf, gdk2, gtk2, Pango,
  GtkInt, GtkProc, GtkGlobals, GtkDef, GtkExtra,
  Classes, InterfaceBase, Types, LCLProc, LCLType, WSMenus, WSLCLClasses,
  LMessages, Graphics, Menus, Forms, LCLIntf;

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
    class procedure SetShortCut(const AMenuItem: TMenuItem; const OldShortCut, NewShortCut: TShortCut); override;
    class procedure SetVisible(const AMenuItem: TMenuItem; const Visible: boolean); override;
    class function SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean; override;
    class function SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean; override;
    class function SetRadioItem(const AMenuItem: TMenuItem; const RadioItem: boolean): boolean; override;
    class function SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean; override;
    class procedure UpdateMenuIcon(const AMenuItem: TMenuItem; const HasIcon: Boolean; const AIcon: TBitmap); override;
  end;

  { TGtk2WSMenu }

  TGtk2WSMenu = class(TWSMenu)
  published
    class procedure SetBiDiMode(const AMenu: TMenu; UseRightToLeftAlign, UseRightToLeftReading : Boolean); override;
  end;

  { TGtk2WSMainMenu }

  TGtk2WSMainMenu = class(TWSMainMenu)
  published
  end;

  { TGtk2WSPopupMenu }

  TGtk2WSPopupMenu = class(TWSPopupMenu)
  published
  end;


implementation

{$I gtkdefines.inc}

function Gtk2MenuItemActivate(widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
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
  and GtkWidgetIsA(Widget, GTK_TYPE_CHECK_MENU_ITEM)
  and (not gtk_check_menu_item_get_active(PGTKCheckMenuItem(Widget))) then Exit;

  FillChar(Mess,SizeOf(Mess),#0);
  Mess.Msg := LM_ACTIVATE;
  Mess.Active:=true;
  Mess.Minimized:=false;
  Mess.ActiveWindow:=0;
  Mess.Result := 0;
  DeliverMessage(Data, Mess);

  Result := CallBackDefaultReturn;
end;

function Gtk2MenuItemToggled(AMenuItem: PGTKCheckMenuItem;
                             AData: gPointer): GBoolean; cdecl;
// AData --> LCLMenuItem: TMenuItem
begin
  Result := CallBackDefaultReturn;
  {$IFDEF EventTrace}
  EventTrace('toggled', AData);
  {$ENDIF}
end;

function Gtk2MenuItemSelect(item: Pointer; AMenuItem: TMenuItem): GBoolean; cdecl;
begin
  AMenuItem.IntfDoSelect;
  Result := CallBackDefaultReturn;
end;

procedure Gtk2MenuItemToggleSizeRequest(AMenuItem: PGtkMenuItem; requisition: Pgint; LCLItem: TMenuItem); cdecl;
var
  spacing: guint;
begin
  if LCLItem.HasIcon then
  begin
    gtk_widget_style_get(PGtkWidget(AMenuItem), 'toggle-spacing', [@spacing, nil]);
    requisition^ := AMenuItem^.toggle_size + spacing;
  end
  else
    GTK_MENU_ITEM_GET_CLASS(AMenuItem)^.toggle_size_request(AMenuItem, requisition);
end;

function Gtk2MenuItemDeselect(item: Pointer; AMenuItem: TMenuItem): GBoolean; cdecl;
begin
  Application.Hint := '';
  Result := CallBackDefaultReturn;
end;

{ TGtk2WSMenuItem }

class procedure TGtk2WSMenuItem.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  // connect activate signal (i.e. clicked)
  g_signal_connect(PGTKObject(AGtkWidget), 'activate',
                   TGTKSignalFunc(@Gtk2MenuItemActivate), AWidgetInfo^.LCLObject);
  g_signal_connect(PGTKObject(AGtkWidget), 'select',
    TGTKSignalFunc(@Gtk2MenuItemSelect), AWidgetInfo^.LCLObject);
  g_signal_connect(PGTKObject(AGtkWidget), 'deselect',
    TGTKSignalFunc(@Gtk2MenuItemDeselect), AWidgetInfo^.LCLObject);
  g_signal_connect(PGTKObject(AGtkWidget), 'toggle-size-request',
    TGTKSignalFunc(@Gtk2MenuItemToggleSizeRequest), AWidgetInfo^.LCLObject);
end;

class procedure TGtk2WSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
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
      TGtkWidgetSet(WidgetSet).RegroupMenuItem(HMENU(PtrUInt(MenuItem)), GroupIndex);
  end;
  //DebugLn('TGtkWidgetSet.AttachMenu END ',AMenuItem.Name,':',AMenuItem.ClassName);
end;

class function TGtk2WSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
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
    // check or radio
    // set 'ShowAlwaysCheckable'
    gtk_check_menu_item_set_show_toggle(PGtkCheckMenuItem(Widget),
      AMenuItem.ShowAlwaysCheckable);
    // set 'Checked'
    gtk_check_menu_item_set_active(PGtkCheckMenuItem(Widget),
      AMenuItem.Checked);

    if (OldCheckMenuItemToggleSize=0) then
    begin
      gtk_menu_item_toggle_size_request(GTK_MENU_ITEM(Widget),
                                        @OldCheckMenuItemToggleSize);
      OldCheckMenuItemToggleSize := GTK_MENU_ITEM(Widget)^.toggle_size;
    end;

    g_signal_connect_after(PGTKObject(Widget), 'toggled',
      TGTKSignalFunc(@Gtk2MenuItemToggled), Pointer(AMenuItem));
  end;

  // set attributes (enabled and rightjustify)
  gtk_widget_set_sensitive(Widget,
                     AMenuItem.Enabled and (AMenuItem.Caption <> cLineCaption));
  if AMenuItem.RightJustify then
    gtk_menu_item_right_justify(PGtkMenuItem(Widget));

  // create the hbox containing the label and the icon
  UpdateInnerMenuItem(AMenuItem, Widget);

  SetCallbacks(Widget, WidgetInfo);

  gtk_widget_show(Widget);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AMenuItem));
  {$ENDIF}
  Result := HMENU(PtrUInt(Widget));
end;

class procedure TGtk2WSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
begin
  { TODO: cleanup }
  TGtkWidgetSet(WidgetSet).DestroyLCLComponent(AMenuItem);
end;

class procedure TGtk2WSMenuItem.SetCaption(const AMenuItem: TMenuItem;
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

class procedure TGtk2WSMenuItem.SetShortCut(const AMenuItem: TMenuItem;
  const OldShortCut, NewShortCut: TShortCut);
//var
  //MenuWidget: PGtkMenuItem;
  //accel_path: String;
  //CurKey: Word;
  //CurShift: TShiftState;
begin
  if not WSCheckMenuItem(AMenuItem, 'SetShortCut') then  Exit;
  
  //DebugLn(['TGtkWSMenuItem.SetShortCut ',dbgsName(AMenuItem),' ',ShortCutToText(NewShortCut)]);

  // Temporary: At least it writes the names of the shortcuts
  UpdateInnerMenuItem(AMenuItem, PGTKWidget(AMenuItem.Handle), NewShortCut);

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
  MenuItemWidget := PGtkWidget(AMenuItem.Handle);
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

class function TGtk2WSMenuItem.SetEnable(const AMenuItem: TMenuItem;
  const Enabled: boolean): boolean;
begin
  Result := False;
  if not WSCheckMenuItem(AMenuItem, 'SetEnable') then
    Exit;
  gtk_widget_set_sensitive(PGtkWidget(AMenuItem.Handle),
                           Enabled and (AMenuItem.Caption <> cLineCaption));
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
  MenuItemWidget := PGtkMenuItem(AMenuItem.Handle);
  gtk_menu_item_set_right_justified(MenuItemWidget, Justified);
  gtk_widget_queue_resize(GTK_WIDGET(MenuItemWidget));
  Result := True;
end;

class procedure TGtk2WSMenuItem.UpdateMenuIcon(const AMenuItem: TMenuItem;
  const HasIcon: Boolean; const AIcon: TBitmap);
begin
  if not WSCheckMenuItem(AMenuItem, 'UpdateMenuIcon') then
    Exit;
  if gtk_is_check_menu_item(Pointer(AMenuItem.Handle)) <> HasIcon then
    AMenuItem.RecreateHandle;
end;

{ TGtk2WSMenu }

class procedure TGtk2WSMenu.SetBiDiMode(const AMenu : TMenu;
  UseRightToLeftAlign, UseRightToLeftReading : Boolean);
{$ifdef GTK_2_8}
const
  MenuDirection : array[Boolean] of Longint = (
    GTK_PACK_DIRECTION_LTR,
    GTK_PACK_DIRECTION_RTL);
{$endif}
begin
{$ifdef GTK_2_8}
  gtk_menu_bar_set_pack_direction(PGtkMenuBar(AMenu.Handle), MenuDirection[UseRightToLeftAlign]);
{$endif}

  if UseRightToLeftReading then
  begin
    // ?
  end
  else
  begin
    // ?
  end;
end;

end.
