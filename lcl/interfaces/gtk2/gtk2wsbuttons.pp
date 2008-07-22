{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSButtons.pp                              * 
 *                             ----------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit Gtk2WSButtons;

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

  { TGtk2WSBitBtn }

  TGtk2WSBitBtn = class(TWSBitBtn)
  private
  protected
  public
  end;

  { TGtk2WSSpeedButton }

  TGtk2WSSpeedButton = class(TWSSpeedButton)
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
//  RegisterWSComponent(TCUstomBitBtn, TGtk2WSBitBtn);
//  RegisterWSComponent(TCustomSpeedButton, TGtk2WSSpeedButton);
////////////////////////////////////////////////////
end.
