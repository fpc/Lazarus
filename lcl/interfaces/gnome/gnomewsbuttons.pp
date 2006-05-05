{ $Id$}
{
 *****************************************************************************
 *                             GnomeWSButtons.pp                             * 
 *                             -----------------                             * 
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
unit GnomeWSButtons;

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

  { TGnomeWSButton }

  TGnomeWSButton = class(TWSButton)
  private
  protected
  public
  end;

  { TGnomeWSBitBtn }

  TGnomeWSBitBtn = class(TWSBitBtn)
  private
  protected
  public
  end;

  { TGnomeWSSpeedButton }

  TGnomeWSSpeedButton = class(TWSSpeedButton)
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
//  RegisterWSComponent(TCustomButton, TGnomeWSButton);
//  RegisterWSComponent(TCustomBitBtn, TGnomeWSBitBtn);
//  RegisterWSComponent(TCustomSpeedButton, TGnomeWSSpeedButton);
////////////////////////////////////////////////////
end.