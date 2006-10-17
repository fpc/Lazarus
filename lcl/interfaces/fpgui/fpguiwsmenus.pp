{ $Id: FpGuiwsmenus.pp 5319 2004-03-17 20:11:29Z marc $}
{
 *****************************************************************************
 *                               FpGuiWSMenus.pp                                * 
 *                               ------------                                * 
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
unit FpGuiWSMenus;

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

  { TFpGuiWSMenuItem }

  TFpGuiWSMenuItem = class(TWSMenuItem)
  private
  protected
  public
  end;

  { TFpGuiWSMenu }

  TFpGuiWSMenu = class(TWSMenu)
  private
  protected
  public
  end;

  { TFpGuiWSMainMenu }

  TFpGuiWSMainMenu = class(TWSMainMenu)
  private
  protected
  public
  end;

  { TFpGuiWSPopupMenu }

  TFpGuiWSPopupMenu = class(TWSPopupMenu)
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
//  RegisterWSComponent(TMenuItem, TFpGuiWSMenuItem);
//  RegisterWSComponent(TMenu, TFpGuiWSMenu);
//  RegisterWSComponent(TMainMenu, TFpGuiWSMainMenu);
//  RegisterWSComponent(TPopupMenu, TFpGuiWSPopupMenu);
////////////////////////////////////////////////////
end.
