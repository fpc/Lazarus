{ $Id$}
{
 *****************************************************************************
 *                              Win32WSMenus.pp                              * 
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
unit Win32WSMenus;

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

  { TWin32WSMenuItem }

  TWin32WSMenuItem = class(TWSMenuItem)
  private
  protected
  public
  end;

  { TWin32WSMenu }

  TWin32WSMenu = class(TWSMenu)
  private
  protected
  public
  end;

  { TWin32WSMainMenu }

  TWin32WSMainMenu = class(TWSMainMenu)
  private
  protected
  public
  end;

  { TWin32WSPopupMenu }

  TWin32WSPopupMenu = class(TWSPopupMenu)
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
//  RegisterWSComponent(TMenuItem, TWin32WSMenuItem);
//  RegisterWSComponent(TMenu, TWin32WSMenu);
//  RegisterWSComponent(TMainMenu, TWin32WSMainMenu);
//  RegisterWSComponent(TPopupMenu, TWin32WSPopupMenu);
////////////////////////////////////////////////////
end.
