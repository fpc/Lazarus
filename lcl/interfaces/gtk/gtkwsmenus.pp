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

  { TGtkWSMenuItem }

  TGtkWSMenuItem = class(TWSMenuItem)
  private
  protected
  public
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

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TMenuItem, TGtkWSMenuItem);
//  RegisterWSComponent(TMenu, TGtkWSMenu);
//  RegisterWSComponent(TMainMenu, TGtkWSMainMenu);
//  RegisterWSComponent(TPopupMenu, TGtkWSPopupMenu);
////////////////////////////////////////////////////
end.
