{  $Id$  }
{
 /***************************************************************************
                            designermenu.pas
                            ----------------


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Martin Patik

}
unit DesignerMenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Menus, Graphics, GraphType, Buttons;

type
  PDesignerMenuItem = ^TDesignerMenuItem;
  TDesignerMenuItem = record
    ParentMenu: PDesignerMenuItem;
    SubMenu: PDesignerMenuItem;
    PrevItem: PDesignerMenuItem;
    NextItem: PDesignerMenuItem;
    Caption: string;
    coord: TRect;
    Level: Integer;
    Selected: Boolean;
    Active: Boolean;
  end;
  
  TDesignerMainMenu = class(TCustomControl)
  private
    FRoot: PDesignerMenuItem;
    FAppMenu: TMainMenu;
  public
    constructor CreateWithMenu(TheOwner: TComponent; AMenu: TMainMenu);
    destructor Destroy; override;
    property Root: PDesignerMenuItem read FRoot write FRoot;
    property AppMenu: TMainMenu read FAppMenu write FAppMenu;
    procedure LoadMainMenu;
    function SaveMainMenu: TMainMenu;
    procedure MouseDownClick(MenuItem: PDesignerMenuItem; x,y: Integer);
    procedure Init(MenuItem: PDesignerMenuItem);
    procedure InitHook(MenuItem: PDesignerMenuItem);
    procedure Link(MenuItem: TMenuItem; ParentM: PDesignerMenuItem);
    procedure Erase(MenuItem: PDesignerMenuItem; C: TCanvas);
    procedure Draw(MenuItem: PDesignerMenuItem; C: TCanvas);
    procedure SetCoordinates(Coord_Left,Coord_Top: Integer;
                             MenuItem: PDesignerMenuItem);
  end;
  

implementation


var 
  temp_level: Integer;

constructor TDesignerMainMenu.CreateWithMenu(TheOwner: TComponent;
  AMenu: TMainMenu);
begin
  inherited Create(TheOwner);
  new(Root);
  AppMenu:=AMenu;
  temp_level:=1;
end;

destructor TDesignerMainMenu.Destroy;
begin
  Dispose(Root);
  inherited Destroy;
end;

procedure TDesignerMainMenu.Init(MenuItem: PDesignerMenuItem);
begin
  MenuItem^.coord.Left:=0;
  MenuItem^.coord.Top:=0;
  MenuItem^.coord.Right:=0;
  MenuItem^.coord.Bottom:=0;
end;

procedure TDesignerMainMenu.InitHook(MenuItem: PDesignerMenuItem);
begin
  MenuItem^.Selected:=false;
  Menuitem^.Active:=false;
  if (MenuItem^.SubMenu <> nil) then InitHook(MenuItem^.SubMenu);
  if (MenuItem^.NextItem <> nil) then InitHook(MenuItem^.NextItem);
end;

procedure TDesignerMainMenu.LoadMainMenu;
var
  prevtemp,temp: PDesignerMenuItem;
  i: Integer;
begin
  prevtemp:=nil;
  for i:= 0 to AppMenu.Items.Count-1 do
  begin
    new(temp);
    temp^.Caption:=AppMenu.Items[i].Caption;
    temp^.Level:=temp_level;
    temp^.NextItem:=nil;
    temp^.SubMenu:=nil;
    temp^.ParentMenu:=nil;
    if (i=0) then
    begin
      temp^.PrevItem:=nil;
      Root:=temp;
    end else
    begin
      temp^.PrevItem:=prevtemp;
      prevtemp^.NextItem:=temp;
    end;
    Init(temp);
    prevtemp:=temp;
    Link(AppMenu.Items[i],temp);
  end;
  InitHook(Root);
  Root^.Selected:=true;
end;

procedure TDesignerMainMenu.Link(MenuItem: TMenuItem; ParentM: PDesignerMenuItem);
var
  prevtemp,temp: PDesignerMenuItem;
  i: Integer;
begin
  inc(temp_level);
  if (MenuItem.Count > 0) then
  begin
    prevtemp:=nil;
    for i:= 0 to MenuItem.Count-1 do
    begin
      new(temp); 
      temp^.Caption:=MenuItem.Items[i].Caption;
      temp^.Level:=temp_level;
      temp^.NextItem:=nil;
      temp^.SubMenu:=nil;
      if (i=0) then
      begin
        temp^.ParentMenu:=ParentM;
        temp^.PrevItem:=nil;
        ParentM^.SubMenu:=temp;
      end else
      begin
        temp^.PrevItem:=prevtemp;
        prevtemp^.NextItem:=temp;
        temp^.ParentMenu:=nil;
      end;
      Init(temp);
      prevtemp:=temp;
      Link(MenuItem.Items[i],temp);
    end;
  end;
  dec(temp_level);
end;

procedure TDesignerMainMenu.Erase(MenuItem: PDesignerMenuItem; C: TCanvas);
begin
  with C do
  begin
    Brush.Color:=clBackground;
    FillRect(Rect(MenuItem^.coord.Left,MenuItem^.coord.Top,MenuItem^.coord.Right,MenuItem^.coord.Bottom));
  end;
  if (MenuItem^.SubMenu <> nil) then Erase(MenuItem^.SubMenu,C);
  if (MenuItem^.NextItem <> nil) then Erase(MenuItem^.NextItem,C);
end;

procedure TDesignerMainMenu.Draw(MenuItem: PDesignerMenuItem; C: TCanvas);
begin
  with C do
  begin
    if (MenuItem^.Selected) then
    begin
      Brush.Color:=clNavy;
      FillRect(Rect(MenuItem^.coord.Left,MenuItem^.coord.Top,MenuItem^.coord.Right,MenuItem^.coord.Bottom));
      Font.color:=clYellow;
    end else
      if (MenuItem^.Active) then
      begin
        Brush.Color:=clGray;
        FillRect(Rect(MenuItem^.coord.Left,MenuItem^.coord.Top,MenuItem^.coord.Right,MenuItem^.coord.Bottom));
        Font.Color:=clBlack;
      end else
      begin
        Brush.Color:=clSilver;
        FillRect(Rect(MenuItem^.coord.Left,MenuItem^.coord.Top,MenuItem^.coord.Right,MenuItem^.coord.Bottom));
        Font.Color:=clBlack;
      end;
    TextOut(MenuItem^.coord.Left + 5,MenuItem^.coord.Top + 3,MenuItem^.Caption);
  end;
  if ((MenuItem^.SubMenu <> nil) and ((MenuItem^.Selected) or (MenuItem^.Active))) then Draw(MenuItem^.SubMenu,C);
  if (MenuItem^.NextItem <> nil) then Draw(MenuItem^.NextItem,C);
end;

function TDesignerMainMenu.SaveMainMenu: TMainMenu;
var
  temp: PDesignerMenuItem;
begin
  Result:=nil;
  temp:=Root;
  while (temp^.NextItem <> nil) do
  begin
  end;
end;

procedure TDesignerMainMenu.SetCoordinates(Coord_Left,Coord_Top: Integer; MenuItem: PDesignerMenuItem);
var
  temp_menuitem: PDesignerMenuItem;
  LongestWidth: Integer;
begin
  if (MenuItem^.Level = 1) then
  begin
    writeln(MenuItem^.Caption,' - ',MenuItem^.Level);
    MenuItem^.coord.Left:=Coord_Left;
    MenuItem^.coord.Top:=Coord_Top;
    MenuItem^.coord.Right:=MenuItem^.coord.Left + Canvas.TextWidth(MenuItem^.Caption) + 10;
    MenuItem^.coord.Bottom:=MenuItem^.coord.Top + Canvas.TextHeight('aaa') + 10;
  end else
  begin
    temp_menuitem:=MenuItem;
    LongestWidth:=Canvas.TextWidth(temp_menuitem^.Caption);
    while (temp_menuitem^.PrevItem <> nil) do temp_menuitem:=temp_menuitem^.PrevItem;
    while (temp_menuitem <> nil) do
    begin
      if (Canvas.TextWidth(temp_menuitem^.Caption) > LongestWidth) then
        LongestWidth:=Canvas.TextWidth(temp_menuitem^.Caption);
      temp_menuitem:=temp_menuitem^.NextItem;
    end;
    MenuItem^.coord.Left:=Coord_Left;
    MenuItem^.coord.Top:=Coord_Top;
    MenuItem^.coord.Right:=MenuItem^.coord.Left + LongestWidth + 10;
    MenuItem^.coord.Bottom:=MenuItem^.coord.Top + Canvas.TextHeight('aaa') + 10;
  end;
  if (MenuItem^.SubMenu <> nil) then
  begin
    if (MenuItem^.Level = 1) then
      SetCoordinates(MenuItem^.coord.Left,MenuItem^.coord.Bottom + 1,MenuItem^.SubMenu)
    else
      SetCoordinates(MenuItem^.coord.Right + 1,MenuItem^.coord.Top,MenuItem^.SubMenu);
  end;
  if (MenuItem^.NextItem <> nil) then 
  begin
    if (MenuItem^.Level = 1) then
      SetCoordinates(MenuItem^.coord.Right + 1,MenuItem^.coord.Top,MenuItem^.NextItem)
    else
      SetCoordinates(MenuItem^.coord.Left,MenuItem^.coord.Bottom + 1,MenuItem^.NextItem); 
  end;
  
end;

procedure TDesignerMainMenu.MouseDownClick(MenuItem: PDesignerMenuItem;
  x,y: Integer);
var
  temp_menuitem: PDesignerMenuItem;
  clickArea: Boolean;
begin
  clickArea:=false;
  if (MenuItem <> nil) then
  begin
    writeln(MenuItem^.Caption,',',MenuItem^.Selected,',',MenuItem^.Active);
    if ((MenuItem^.Selected) or (MenuItem^.Active)) then clickArea:=true;
    if ((MenuItem^.coord.Left <= x) and (MenuItem^.coord.Right >= x) and
        (MenuItem^.coord.Top <= y) and (MenuItem^.coord.Bottom >= y)) then 
    begin
      InitHook(Root);
      MenuItem^.Selected:=true;
      temp_menuitem:=MenuItem;
      while ((temp_menuitem^.ParentMenu <> nil) or (temp_menuitem^.PrevItem <> nil)) do
      begin
        if (temp_menuitem^.ParentMenu <> nil) then
	begin
	  temp_menuitem^.ParentMenu^.Active:=true;
	  temp_menuitem:=temp_menuitem^.ParentMenu;
	end else
	  if (temp_menuitem^.PrevItem <> nil) then temp_menuitem:=temp_menuitem^.PrevItem;
      end;
    end;
    if (clickArea) then MouseDownClick(MenuItem^.SubMenu,x,y);
    MouseDownClick(MenuItem^.NextItem,x,y);
  end;
end;
 
end.

