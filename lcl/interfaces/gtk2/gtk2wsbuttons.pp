{ $Id$}
{
 *****************************************************************************
 *                             gtk2wsbuttons.pp                              * 
 *                             ----------------                              * 
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
unit gtk2wsbuttons;

{$mode objfpc}{H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as litle as posible circles,
// Uncomment only when needed for registration
////////////////////////////////////////////////////
//  buttons,
////////////////////////////////////////////////////
  wsbuttons, wslclclasses;

type

  { TGtk2WSButton }

  TGtk2WSButton = class(TWSButton)
  private
  protected
  public
  end;

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
//  RegisterWSComponent(TButton, TGtk2WSButton);
//  RegisterWSComponent(TBitBtn, TGtk2WSBitBtn);
//  RegisterWSComponent(TSpeedButton, TGtk2WSSpeedButton);
////////////////////////////////////////////////////
end.
