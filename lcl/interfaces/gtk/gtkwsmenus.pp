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
 *  See the file COPYING.LCL, included in this distribution,                 *
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
  Classes, InterfaceBase, LCLType, WSMenus, WSLCLClasses,
  {$IFDEF gtk2}
  glib2, gdk2pixbuf, gdk2, gtk2, Pango,
  {$ELSE}
  glib, gdk, gtk, {$Ifndef NoGdkPixbufLib}gdkpixbuf,{$EndIf}
  {$ENDIF}
  GtkInt, gtkproc, gtkglobals, Menus;

type

  { TGtkWSMenuItem }

  TGtkWSMenuItem = class(TWSMenuItem)
  private
  protected
  public
    class procedure AttachMenu(const AMenuItem: TMenuItem); override;
    class procedure SetCaption(const AMenuItem: TMenuItem; const ACaption: string); override;
  end;

  { TGtkWSMenu }

  TGtkWSMenu = class(TWSMenu)
  private
  protected
  public
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
  public
  end;


implementation

procedure TGtkWSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
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
      RaiseException('TGtkWidgetSet.AttachMenu Handle=0');
    ParentMenuWidget := PGtkWidget(Parent.Handle);
    if ParentMenuWidget=nil then
      RaiseException('TGtkWidgetSet.AttachMenu ParentMenuWidget=nil');

    if GtkWidgetIsA(ParentMenuWidget,GTK_TYPE_MENU_BAR) then begin
      // mainmenu (= a menu bar)
      ContainerMenu:=ParentMenuWidget;
      gtk_menu_bar_insert(ParentMenuWidget,MenuItem, AMenuItem.MenuIndex);
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
      gtk_menu_insert(ContainerMenu, MenuItem, AMenuItem.MenuIndex);
    end;

    SetContainerMenuToggleSize;

    if GtkWidgetIsA(MenuItem, GTK_TYPE_RADIO_MENU_ITEM) then
      TGtkWidgetSet(InterfaceObject).RegroupMenuItem(HMENU(MenuItem),GroupIndex);
  end;
  //DebugLn('TGtkWidgetSet.AttachMenu END ',AMenuItem.Name,':',AMenuItem.ClassName);
end;

procedure TGtkWSMenuItem.SetCaption(const AMenuItem: TMenuItem; const ACaption: string);
var
  MenuItemWidget: PGtkWidget;
begin
  if not AMenuItem.HandleAllocated then exit;
  MenuItemWidget:=PGtkWidget(AMenuItem.Handle);
  UpdateInnerMenuItem(AMenuItem,MenuItemWidget);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TMenuItem, TGtkWSMenuItem);
//  RegisterWSComponent(TMenu, TGtkWSMenu);
//  RegisterWSComponent(TMainMenu, TGtkWSMainMenu);
//  RegisterWSComponent(TPopupMenu, TGtkWSPopupMenu);
////////////////////////////////////////////////////
end.
