{ $Id$}
{
 *****************************************************************************
 *                             Win32WSButtons.pp                             * 
 *                             -----------------                             * 
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
unit Win32WSButtons;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  Buttons,
////////////////////////////////////////////////////
  WSButtons, WSLCLClasses;

type

  { TWin32WSButton }

  TWin32WSButton = class(TWSButton)
  private
  protected
  public
  end;

  { TWin32WSBitBtn }

  TWin32WSBitBtn = class(TWSBitBtn)
  private
  protected
  public
  end;

  { TWin32WSSpeedButton }

  TWin32WSSpeedButton = class(TWSSpeedButton)
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
//  RegisterWSComponent(TCustomButton, TWin32WSButton);
//  RegisterWSComponent(TCustomBitBtn, TWin32WSBitBtn);
//  RegisterWSComponent(TCustomSpeedButton, TWin32WSSpeedButton);
////////////////////////////////////////////////////
end.
