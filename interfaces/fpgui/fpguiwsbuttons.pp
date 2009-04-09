{ $Id: FpGuiwsbuttons.pp 5682 2004-07-15 10:43:39Z mattias $}
{
 *****************************************************************************
 *                              FpGuiWSButtons.pp                               * 
 *                              --------------                               * 
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
unit FpGuiWSButtons;

{$mode objfpc}{$H+}

interface

uses
  // Bindings
  fpguiwsprivate,
  // LCL
  Buttons, LCLType, Controls,
  // Widgetset
  WSButtons, WSLCLClasses;

type

  { TFpGuiWSBitBtn }

  TFpGuiWSBitBtn = class(TWSBitBtn)
  private
  protected
  public
  end;

  { TFpGuiWSSpeedButton }

  TFpGuiWSSpeedButton = class(TWSSpeedButton)
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
//  RegisterWSComponent(TCustomBitBtn, TFpGuiWSBitBtn);
//  RegisterWSComponent(TCustomSpeedButton, TFpGuiWSSpeedButton);
////////////////////////////////////////////////////
end.
