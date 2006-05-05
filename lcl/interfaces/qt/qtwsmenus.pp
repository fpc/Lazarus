{ $Id$}
{
 *****************************************************************************
 *                               QtWSMenus.pp                                * 
 *                               ------------                                * 
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
unit QtWSMenus;

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

  { TQtWSMenuItem }

  TQtWSMenuItem = class(TWSMenuItem)
  private
  protected
  public
  end;

  { TQtWSMenu }

  TQtWSMenu = class(TWSMenu)
  private
  protected
  public
  end;

  { TQtWSMainMenu }

  TQtWSMainMenu = class(TWSMainMenu)
  private
  protected
  public
  end;

  { TQtWSPopupMenu }

  TQtWSPopupMenu = class(TWSPopupMenu)
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
//  RegisterWSComponent(TMenuItem, TQtWSMenuItem);
//  RegisterWSComponent(TMenu, TQtWSMenu);
//  RegisterWSComponent(TMainMenu, TQtWSMainMenu);
//  RegisterWSComponent(TPopupMenu, TQtWSPopupMenu);
////////////////////////////////////////////////////
end.