{ $Id$}
{
 *****************************************************************************
 *                              gnomewsmenus.pp                              * 
 *                              ---------------                              * 
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
unit gnomewsmenus;

{$mode objfpc}{H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as litle as posible circles,
// Uncomment only when needed for registration
////////////////////////////////////////////////////
//  menus,
////////////////////////////////////////////////////
  wsmenus, wslclclasses;

type

  { TGnomeWSMenuItem }

  TGnomeWSMenuItem = class(TWSMenuItem)
  private
  protected
  public
  end;

  { TGnomeWSMenu }

  TGnomeWSMenu = class(TWSMenu)
  private
  protected
  public
  end;

  { TGnomeWSMainMenu }

  TGnomeWSMainMenu = class(TWSMainMenu)
  private
  protected
  public
  end;

  { TGnomeWSPopupMenu }

  TGnomeWSPopupMenu = class(TWSPopupMenu)
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
//  RegisterWSComponent(TMenuItem, TGnomeWSMenuItem);
//  RegisterWSComponent(TMenu, TGnomeWSMenu);
//  RegisterWSComponent(TMainMenu, TGnomeWSMainMenu);
//  RegisterWSComponent(TPopupMenu, TGnomeWSPopupMenu);
////////////////////////////////////////////////////
end.
