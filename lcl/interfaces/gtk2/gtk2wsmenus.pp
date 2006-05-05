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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  Menus,
////////////////////////////////////////////////////
  WSMenus, WSLCLClasses;

type

  { TGtk2WSMenuItem }

  TGtk2WSMenuItem = class(TWSMenuItem)
  private
  protected
  public
  end;

  { TGtk2WSMenu }

  TGtk2WSMenu = class(TWSMenu)
  private
  protected
  public
  end;

  { TGtk2WSMainMenu }

  TGtk2WSMainMenu = class(TWSMainMenu)
  private
  protected
  public
  end;

  { TGtk2WSPopupMenu }

  TGtk2WSPopupMenu = class(TWSPopupMenu)
  private
  protected
  public
  end;


implementation

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TMenuItem, TGtk2WSMenuItem);
//  RegisterWSComponent(TMenu, TGtk2WSMenu);
//  RegisterWSComponent(TMainMenu, TGtk2WSMainMenu);
//  RegisterWSComponent(TPopupMenu, TGtk2WSPopupMenu);
////////////////////////////////////////////////////
end.