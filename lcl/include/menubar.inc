// included by menus.pp
{
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
{---------------------------------------------------------------------------------------------}
constructor TMenuBar.Create(AOwner: TComponent);
begin
   Inherited Create(AOwner);
end;

{-----------------------------------------------------------------------------}
procedure TMenuBar.Show;
begin

end;

{---------------------------------------------------------------------------------------------}
procedure TMenuBar.AddMenu(Title: TCaption; Menu: TMenu);
begin
//   fMenu := TMenuItem.Create(nil);
//   fMenu.Caption := Title;
//   fMenu.show;

// ******************************
// the following line has been commented out so that gtk dependency can be
// removed from all files. The following function needs to be moved.   MAH
//
//   gtk_menu_item_set_submenu(GTK_MENU_ITEM(TControl(fMenu).fComponent), TControl(Menu).fComponent);
//   gtk_menu_bar_append(GTK_MENU_BAR(fComponent), TControl(fMenu).fComponent);
end;

// included by menus.pp

