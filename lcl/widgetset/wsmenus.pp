{ $Id$}
{
 *****************************************************************************
 *                                WSMenus.pp                                 * 
 *                                ----------                                 * 
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
unit WSMenus;

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
  WSLCLClasses;

type

  { TWSMenuItem }

  TWSMenuItem = class(TWSLCLComponent)
  private
  protected
  public
  end;

  { TWSMenu }

  TWSMenu = class(TWSLCLComponent)
  private
  protected
  public
  end;

  { TWSMainMenu }

  TWSMainMenu = class(TWSMenu)
  private
  protected
  public
  end;

  { TWSPopupMenu }

  TWSPopupMenu = class(TWSMenu)
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
//  RegisterWSComponent(TMenuItem, TWSMenuItem);
//  RegisterWSComponent(TMenu, TWSMenu);
//  RegisterWSComponent(TMainMenu, TWSMainMenu);
//  RegisterWSComponent(TPopupMenu, TWSPopupMenu);
////////////////////////////////////////////////////
end.
