{***************************************************************************
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
}
unit Options_Compiler_Conditionals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ComCtrls, Menus,
  compop;

type

  { TCompOptsConditionalsFrame }

  TCompOptsConditionalsFrame = class(TFrame)
    COCTreeView: TTreeView;
    COCPopupMenu: TPopupMenu;
    InsertAboveMenuItem: TMenuItem;
    InsertBelowMenuItem: TMenuItem;
    InsertChildMenuItem: TMenuItem;
    DeleteMenuItem: TMenuItem;
    PropertiesMenuItem: TMenuItem;
    MoveLvlDownMenuItem: TMenuItem;
    MoveLvlUpMenuItem: TMenuItem;
    MoveDownMenuItem: TMenuItem;
    MoveUpMenuItem: TMenuItem;
  private
  public
  end;

implementation

initialization
  {$I options_compiler_conditionals.lrs}

end.

